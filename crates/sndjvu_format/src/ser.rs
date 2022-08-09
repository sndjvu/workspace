#![deny(clippy::integer_arithmetic)]

use crate::annot::Annot;
use core::mem::replace;
use alloc::string::String;
use alloc::vec::Vec;
#[cfg(sndjvu_backtrace)]
use std::backtrace::Backtrace;

const CHUNK_HEADER_SIZE: u32 = 8;
const COMPONENT_HEADER_SIZE: u32 = 12;

pub struct OverflowError;

#[derive(Debug)]
enum ErrorKind {
    Overflow,
    #[cfg(feature = "std")]
    Io(std::io::Error),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    #[cfg(sndjvu_backtrace)]
    backtrace: std::backtrace::Backtrace,
}

impl Error {
    #[cfg(feature = "std")]
    fn io(e: std::io::Error) -> Self {
        Self {
            kind: ErrorKind::Io(e),
            #[cfg(sndjvu_backtrace)]
            backtrace: std::backtrace::Backtrace::capture(),
        }
    }
}

impl From<OverflowError> for Error {
    fn from(_: OverflowError) -> Self {
        Self {
            kind: ErrorKind::Overflow,
            #[cfg(sndjvu_backtrace)]
            backtrace: Backtrace::capture(),
        }
    }
}

macro_rules! checked_sum {
    ( $( $a:expr ),* $( , )? ) => {
        (|| -> Result<u32, Error> {
            let mut total = 0u32;
            $(
                total = total.checked_add($a).ok_or(OverflowError)?;
            )*
            Ok(total)
        })()
    };
}

trait Out: core::fmt::Debug {
    fn put(&mut self, b: &[u8]) -> Result<(), Error>;
}

// TODO auto traits
type ErasedOutMut<'wr> = &'wr mut (dyn Out + 'wr);

#[derive(Debug)]
struct VecOut(Vec<u8>);

impl Out for VecOut {
    fn put(&mut self, b: &[u8]) -> Result<(), Error> {
        self.0.extend_from_slice(b);
        Ok(())
    }
}

#[cfg(feature = "std")]
impl<W: std::io::Write + core::fmt::Debug> Out for W {
    fn put(&mut self, b: &[u8]) -> Result<(), Error> {
        self.write_all(b).map_err(Error::io)?;
        Ok(())
    }
}

macro_rules! out {
    ( $o:expr ; $( $b:expr ),* $( , )? ) => {
        (|| -> Result<(), Error> {
            $( $o.put(::core::convert::AsRef::as_ref(&$b))?; )*
            Ok(())
        })()
    };
}

// Note: the sizes stored in this type are as they appear in (the compressed portion of)
// the DIRM chunk, so they include the 12 bytes FORM:$len:$kind
#[derive(Debug)]
struct ComponentSizes {
    buf: Vec<[u8; 3]>,
}

impl ComponentSizes {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }

    fn push(&mut self, size: u32) -> Result<(), Error> {
        let size = checked_sum!(size, COMPONENT_HEADER_SIZE)?;
        if let [0, b1, b2, b3] = size.to_be_bytes() {
            self.buf.push([b1, b2, b3]);
            Ok(())
        } else {
            Err(OverflowError.into())
        }
    }

    fn as_bytes(&self) -> &[u8] {
        crate::shim::arrays_as_slice(&self.buf)
    }

    fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.buf.iter().map(|&[b1, b2, b3]| u32::from_be_bytes([0, b1, b2, b3]))
    }

    fn into_lengths(self) -> ComponentLengths {
        ComponentLengths { inner: self.buf.into_iter() }
    }
}

#[derive(Debug)]
struct ComponentLengths {
    inner: <Vec<[u8; 3]> as IntoIterator>::IntoIter,
}

impl Iterator for ComponentLengths {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        // yielded values are suitable for the length field of the component header:
        // the four kind bytes are included, but not the FORM:$len
        self.inner.next().map(|[b1, b2, b3]| u32::from_be_bytes([0, b1, b2, b3]) - 8)
    }
}

#[derive(Debug)]
struct ComponentMeta {
    flags_buf: Vec<u8>,
    ids_buf: Vec<u8>,
}

impl ComponentMeta {
    fn new() -> Self {
        Self {
            flags_buf: Vec::new(),
            ids_buf: Vec::new(),
        }
    }

    fn push(&mut self, kind: crate::ComponentKind, id: &str) {
        self.flags_buf.push(kind as u8);
        self.ids_buf.extend_from_slice(id.as_bytes());
        self.ids_buf.push(b'\0');
    }
}

pub struct Okay {
    num_components: u16,
    dirm_data: Vec<u8>,
    chunk_lens: Vec<u32>,
    component_sizes: ComponentSizes,
    total: u32,
}

impl Okay {
    fn dummy() -> Self {
        Self {
            num_components: 0,
            dirm_data: Vec::new(),
            chunk_lens: Vec::new(),
            component_sizes: ComponentSizes::new(),
            total: 0,
        }
    }
}

#[derive(Debug)]
enum Cur {
    // empty document, neither start_component nor start_chunk called
    Start,
    // empty component, start_component called but not start_chunk
    InComponent,
    // non-empty component, start_component and start_chunk called
    InChunk {
        // count of bytes in the content of the current (in-progress) chunk
        chunk: u32,
        // count of bytes in the content of the current component,
        // not including the chunk in progress or the header of that chunk,
        // but including the padding before that chunk, if any
        // (why this specific accounting? idk, it just makes sense to me)
        component: u32,
    },
}

#[derive(Debug)]
struct First {
    chunk_lens: Vec<u32>,
    component_sizes: ComponentSizes,
    component_meta: ComponentMeta,
    num_components: u16,
    cur: Cur,
    // when Cur::Start, this is 16 (magic + document header)
    // when Cur::InComponent, this is the count of bytes up to the end of the header
    // of the current (in-progress) component
    // when Cur::InChunk, this is the count of bytes up to the beginning of the header
    // of the current chunk
    total: u32,
}

#[derive(Debug)]
struct Second<'wr> {
    chunk_lens: <Vec<u32> as IntoIterator>::IntoIter,
    component_lens: ComponentLengths,
    // the number of bytes written so far, updated with every write
    running: u32,
    out: ErasedOutMut<'wr>,
}

impl<'wr> Second<'wr> {
    fn put(&mut self, b: &[u8]) -> Result<(), Error> {
        self.out.put(b)?;
        self.running += b.len() as u32;
        Ok(())
    }
}

#[derive(Debug)]
enum Stage<'wr> {
    First(First),
    Second(Second<'wr>),
}

impl<'wr> Stage<'wr> {
    fn put(&mut self, b: &[u8]) -> Result<(), Error> {
        match *self {
            Stage::First(ref mut first) => {
                // in practice it seems we only use this to write chunk data,
                // which means calling it without Cur::InChunk is a bug
                // the behavior is to forward to the underlying Out and also
                // increment the chunk counter
                let len: u32 = b.len().try_into().map_err(|_| OverflowError)?;
                match first.cur {
                    Cur::InChunk { ref mut chunk, .. } => *chunk += len,
                    _ => unreachable!(),
                }
            }
            Stage::Second(ref mut second) => second.put(b)?,
        }
        Ok(())
    }

    fn start_component(&mut self, kind: crate::ComponentKind, id: &str) -> Result<(), Error> {
        match *self {
            Self::First(ref mut first) => {
                match replace(&mut first.cur, Cur::InComponent) {
                    Cur::Start => {},
                    Cur::InComponent => {
                        // previous component was empty: record its size as 12 bytes (just the header)
                        first.component_sizes.push(COMPONENT_HEADER_SIZE).unwrap();
                    }
                    Cur::InChunk { chunk, mut component } => {
                        first.chunk_lens.push(chunk);
                        // final chunk of the previous component
                        component = checked_sum!(component, CHUNK_HEADER_SIZE, chunk)?;
                        first.total = checked_sum!(first.total, CHUNK_HEADER_SIZE, chunk)?;
                        first.component_sizes.push(component)?;
                        // padding before the new component
                        if first.total % 2 != 0 {
                            first.total = checked_sum!(first.total, 1)?;
                        } 
                    }
                }

                first.num_components = first.num_components.checked_add(1).ok_or(OverflowError)?;
                first.total = checked_sum!(first.total, COMPONENT_HEADER_SIZE)?;
                first.component_meta.push(kind, id);
            }
            Self::Second(ref mut second) => {
                if second.running % 2 != 0 {
                    second.running += 1;
                    out!(second; [0])?;
                }
                second.running += COMPONENT_HEADER_SIZE;
                let len = second.component_lens.next().ok_or(OverflowError)?;
                out!(second; b"FORM", len.to_be_bytes(), kind.name())?;
            }
        }
        Ok(())
    }

    fn start_chunk(&mut self, id: &[u8; 4]) -> Result<(), Error> {
        match *self {
            Self::First(ref mut first) => {
                let component = match first.cur {
                    Cur::Start => unreachable!(),
                    Cur::InComponent => 0,
                    Cur::InChunk { chunk, mut component } => {
                        first.chunk_lens.push(chunk);
                        first.total = checked_sum!(first.total, CHUNK_HEADER_SIZE, chunk)?;
                        component = checked_sum!(component, CHUNK_HEADER_SIZE, chunk)?;
                        // padding before the new chunk
                        if first.total % 2 != 0 {
                            first.total = checked_sum!(first.total, 1)?;
                            component = checked_sum!(component, 1)?;
                        }
                        component
                    }
                };
                first.cur = Cur::InChunk { chunk: 0, component };
            }
            Self::Second(ref mut second) => {
                if second.running % 2 != 0 {
                    second.running += 1;
                    out!(second; [0])?;
                }
                second.running += CHUNK_HEADER_SIZE;
                let len = second.chunk_lens.next().unwrap();
                out!(second; id, len.to_be_bytes())?;
            }
        }
        Ok(())
    }
}

enum SerializerRepr<'wr> {
    First(First),
    Second(SerializeMultiPageHead<'wr>),
}

pub struct Serializer<'wr> {
    repr: SerializerRepr<'wr>,
}

pub trait Serialize {
    fn serialize(&self, serializer: Serializer<'_>) -> Result<Okay, Error>;
}

impl<'wr> Serializer<'wr> {
    fn first_stage() -> Self {
        Self {
            repr: SerializerRepr::First(First {
                chunk_lens: Vec::new(),
                component_sizes: ComponentSizes::new(),
                component_meta: ComponentMeta::new(),
                num_components: 0,
                total: 0,
                cur: Cur::Start,
            }),
        }
    }

    fn second_stage(okay: Okay, out: ErasedOutMut<'wr>) -> Self {
        let Okay { num_components, dirm_data, chunk_lens, component_sizes, total } = okay;
        let _ = total; // XXX
        Self {
            repr: SerializerRepr::Second(SerializeMultiPageHead {
                num_components,
                dirm_data,
                chunk_lens,
                component_sizes,
                out,
            }),
        }
    }

    /// Begin serializing a bundled multi-page document.
    pub fn multi_page_bundled(self) -> SerializeMultiPageBundled<'wr> {
        match self.repr {
            SerializerRepr::First(first) => SerializeMultiPageBundled::Components(
                SerializeComponents { stage: Stage::First(first) },
            ),
            SerializerRepr::Second(compress_dirm) => SerializeMultiPageBundled::Head(compress_dirm),
        }
    }

    // TODO other document formats
}

pub enum SerializeMultiPageBundled<'wr> {
    Head(SerializeMultiPageHead<'wr>),
    Components(SerializeComponents<'wr>),
}

pub struct SerializeMultiPageHead<'wr> {
    num_components: u16,
    dirm_data: Vec<u8>,
    chunk_lens: Vec<u32>,
    component_sizes: ComponentSizes,
    out: ErasedOutMut<'wr>,
}

impl<'wr> SerializeMultiPageHead<'wr> {
    pub fn for_compression(&self) -> &[u8] {
        &self.dirm_data
    }

    // FIXME name
    pub fn dirm_and_navm(self, compressed: &[u8], navm: Option<&[u8]>) -> Result<SerializeComponents<'wr>, Error> {
        // accumulate offsets
        let mut off: u32 = 4 + 4 + 4 + 4; // AT&T:FORM:$len:DJVM
        off += 4 + 4; // DIRM:$len
        let mut dirm_len = 1 + 2; // $flags:$num_components
        let addl = 4u32.checked_mul(self.num_components as u32).ok_or(OverflowError)?;
        dirm_len = checked_sum!(dirm_len, addl)?;
        let addl: u32 = compressed.len().try_into().map_err(|_| OverflowError)?;
        dirm_len = checked_sum!(dirm_len, addl)?;
        off = checked_sum!(off, dirm_len)?;
        if off % 2 != 0 {
            off = checked_sum!(off, 1)?;
        }
        if let Some(navm) = navm {
            let addl: u32 = navm.len().try_into().map_err(|_| OverflowError)?;
            off = checked_sum!(off, 8, addl)?;
        }
        let running = off; // save for later
        let mut offsets = Vec::new();
        for size in self.component_sizes.iter() {
            if off % 2 != 0 {
                off += 1;
            }
            offsets.push(off.to_be_bytes());
            off += size;
        }
        let full_len = off - 12; // XXX
        
        out!(
            self.out;
            b"AT&T", b"FORM", full_len.to_be_bytes(), b"DJVM",
            b"DIRM", dirm_len.to_be_bytes(),
            crate::DirmVersion::CURRENT.pack(crate::IsBundled::Yes), self.num_components.to_be_bytes(),
            crate::shim::arrays_as_slice(&offsets),
            compressed,
        )?;
        if let Some(navm) = navm {
            if dirm_len % 2 != 0 {
                out!(self.out; [0])?;
            }
            out!(self.out; b"NAVM", u32::to_be_bytes(navm.len() as _), navm)?;
        }

        Ok(SerializeComponents {
            stage: Stage::Second(Second {
                chunk_lens: self.chunk_lens.into_iter(),
                component_lens: self.component_sizes.into_lengths(),
                running,
                out: self.out,
            })
        })
    }
}

#[derive(Debug)]
pub struct SerializeComponents<'wr> {
    stage: Stage<'wr>,
}

impl<'wr> SerializeComponents<'wr> {
    pub fn djvi(&mut self, id: &str) -> Result<SerializeElements<'_, 'wr>, Error> {
        self.stage.start_component(crate::ComponentKind::Djvi, id)?;
        Ok(SerializeElements { stage: &mut self.stage })
    }

    pub fn djvu(
        &mut self,
        id: &str,
        width: u16,
        height: u16,
        dpi: u16,
        gamma: u8,
        rotation: crate::PageRotation,
    ) -> Result<SerializeElements<'_, 'wr>, Error> {
        self.stage.start_component(crate::ComponentKind::Djvu, id)?;
        self.stage.start_chunk(b"INFO")?;
        out!(
            self.stage;
            width.to_be_bytes(),
            height.to_be_bytes(),
            crate::InfoVersion::CURRENT.pack(),
            dpi.to_le_bytes(),
            [gamma],
            [rotation as u8],
        )?;
        Ok(SerializeElements { stage: &mut self.stage })
    }

    pub fn thum(&mut self, id: &str) -> Result<SerializeThumbnails<'_, 'wr>, Error> {
        self.stage.start_component(crate::ComponentKind::Thum, id)?;
        Ok(SerializeThumbnails { stage: &mut self.stage })
    }

    pub fn finish(self) -> Result<Okay, Error> {
        match self.stage {
            Stage::First(First {
                mut chunk_lens,
                mut component_sizes,
                component_meta,
                num_components,
                cur,
                mut total,
            }) => {
                match cur {
                    Cur::Start => {},
                    Cur::InComponent => component_sizes.push(0).unwrap(),
                    Cur::InChunk { mut component, chunk } => {
                        chunk_lens.push(chunk);
                        // final chunk of the previous component
                        component = checked_sum!(component, CHUNK_HEADER_SIZE, chunk)?;
                        total = checked_sum!(total, CHUNK_HEADER_SIZE, chunk)?;
                        component_sizes.push(component)?;
                    }
                }
                let mut dirm_data = Vec::with_capacity(
                    component_sizes.as_bytes().len() +
                    component_meta.flags_buf.len() +
                    component_meta.ids_buf.len(),
                );
                dirm_data.extend_from_slice(component_sizes.as_bytes());
                dirm_data.extend_from_slice(&component_meta.flags_buf);
                dirm_data.extend_from_slice(&component_meta.ids_buf);
                Ok(Okay {
                    num_components,
                    dirm_data,
                    chunk_lens,
                    component_sizes,
                    total,
                })
            }
            Stage::Second(_) => Ok(Okay::dummy()),
        }
    }
}

pub struct SerializeElements<'co, 'wr: 'co> {
    stage: &'co mut Stage<'wr>,
}

impl<'co, 'wr: 'co> SerializeElements<'co, 'wr> {
    pub fn anta(&mut self, ant: &Ant) -> Result<(), Error> {
        self.stage.start_chunk(b"ANTa")?;
        out!(self.stage; ant.raw)?;
        Ok(())
    }

    pub fn antz(&mut self, bzz: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"ANTz")?;
        out!(self.stage; bzz)?;
        Ok(())
    }

    pub fn txta(&mut self, text: &str, zones: &Zones) -> Result<(), Error> {
        let len: U24 = text.len().try_into()?;
        if !zones.0.stack.is_empty() {
            panic!()
        }
        self.stage.start_chunk(b"TXTa")?;
        out!(
            self.stage;
            len.to_be_bytes(),
            text,
            crate::TxtVersion::CURRENT.pack(),
            zones.0.raw,
        )?;
        Ok(())
    }

    pub fn txtz(&mut self, bzz: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"TXTz")?;
        out!(self.stage; bzz)?;
        Ok(())
    }

    pub fn djbz(&mut self, jb2: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"Djbz")?;
        out!(self.stage; jb2)?;
        Ok(())
    }

    pub fn sjbz(&mut self, jb2: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"Sjbz")?;
        out!(self.stage; jb2)?;
        Ok(())
    }

    pub fn fg44(
        &mut self,
        num_slices: u8,
        color_space: crate::Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: crate::Cdc,
        iw44: &[u8],
    ) -> Result<(), Error> {
        self.stage.start_chunk(b"FG44")?;
        out!(
            self.stage;
            [0], // serial number
            [num_slices],
            crate::Iw44Version::CURRENT.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc.get()],
            iw44,
        )?;
        Ok(())
    }

    pub fn bg44(
        &mut self,
        num_slices: u8,
        color_space: crate::Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: crate::Cdc,
        iw44: &[u8],
    ) -> Result<SerializeBg44Chunks<'_, 'wr>, Error> {
        self.stage.start_chunk(b"BG44")?;
        out!(
            self.stage;
            [0], // serial number
            [num_slices],
            crate::Iw44Version::CURRENT.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc.get()],
            iw44,
        )?;
        Ok(SerializeBg44Chunks {
            serial: 1,
            stage: &mut self.stage,
        })
    }

    // TODO FGbz

    pub fn incl(&mut self, target_id: &str) -> Result<(), Error> {
        self.stage.start_chunk(b"INCL")?;
        out!(self.stage; target_id.as_bytes())?;
        Ok(())
    }

    pub fn bgjp(&mut self, jpeg: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"BGjp")?;
        out!(self.stage; jpeg)?;
        Ok(())
    }

    pub fn fgjp(&mut self, jpeg: &[u8]) -> Result<(), Error> {
        self.stage.start_chunk(b"FGjp")?;
        out!(self.stage; jpeg)?;
        Ok(())
    }

    // TODO Smmr
}

pub struct SerializeBg44Chunks<'el, 'wr: 'el> {
    serial: u8,
    stage: &'el mut Stage<'wr>,
}

impl<'el, 'wr: 'el> SerializeBg44Chunks<'el, 'wr> {
    pub fn chunk(
        &mut self,
        num_slices: u8,
        iw44: &[u8],
    ) -> Result<(), Error> {
        self.stage.start_chunk(b"BG44")?;
        out!(
            self.stage;
            [self.serial],
            [num_slices],
            iw44,
        )?;
        self.serial += 1;
        Ok(())
    }
}

pub struct SerializeThumbnails<'co, 'wr: 'co> {
    stage: &'co mut Stage<'wr>,
}

impl<'co, 'wr: 'co> SerializeThumbnails<'co, 'wr> {
    pub fn th44(
        &mut self,
        num_slices: u8,
        color_space: crate::Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: crate::Cdc,
        iw44: &[u8],
    ) -> Result<(), Error> {
        self.stage.start_chunk(b"TH44")?;
        let version = crate::Iw44Version::CURRENT;
        out!(
            self.stage;
            [0], // serial number
            [num_slices],
            version.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc.get()],
            iw44,
        )?;
        Ok(())
    }
}

#[cfg(feature = "std")]
pub fn to_writer<T: Serialize, W: std::io::Write + core::fmt::Debug>(doc: &T, mut writer: W) -> Result<(), Error> {
    let serializer = Serializer::first_stage();
    let okay = doc.serialize(serializer)?;
    let serializer = Serializer::second_stage(okay, &mut writer as _);
    let _okay = doc.serialize(serializer)?;
    Ok(())
}

pub fn to_vec<T: Serialize>(doc: &T) -> Result<Vec<u8>, Error> {
    let serializer = Serializer::first_stage();
    let okay = doc.serialize(serializer)?;
    let mut buf = VecOut(Vec::with_capacity(okay.total as _));
    let serializer = Serializer::second_stage(okay, &mut buf as _);
    let _okay = doc.serialize(serializer)?;
    Ok(buf.0)
}

pub struct Ant {
    raw: String,
}

impl Ant {
    pub fn add(&mut self, annot: &Annot) {
        use core::fmt::Write;
        if self.raw.is_empty() {
            let _ = write!(self.raw, "{annot}");
        } else {
            let _ = write!(self.raw, " {annot}");
        }
    }

    pub fn bytes(&self) -> &[u8] {
        self.raw.as_bytes()
    }
}

pub struct U24(u32);

impl U24 {
    fn to_be_bytes(self) -> [u8; 3] {
        let [_, b1, b2, b3] = self.0.to_be_bytes();
        [b1, b2, b3]
    }

    fn inc(&mut self) -> Result<(), OverflowError> {
        if self.0 + 1 < 1 << 24 {
            self.0 += 1;
            Ok(())
        } else {
            Err(OverflowError)
        }
    }
}

impl TryFrom<usize> for U24 {
    type Error = OverflowError;

    fn try_from(x: usize) -> Result<Self, Self::Error> {
        let x: u32 = x.try_into().map_err(|_| OverflowError)?;
        if let [0, _, _, _] = x.to_be_bytes() {
            Ok(Self(x))
        } else {
            Err(OverflowError)
        }
    }
}

#[derive(Default)]
struct AddingZones {
    raw: Vec<u8>,
    stack: Vec<(usize, U24)>,
}

impl AddingZones {
    fn start_zone(
        &mut self,
        kind: crate::ZoneKind,
        offset_x: i16,
        offset_y: i16,
        width: i16,
        height: i16,
        text_len: U24,
    ) -> Result<(), OverflowError> {
        fn cvt(n: i16) -> u16 {
            (n as u16) ^ (1 << 15)
        }

        if let Some(&mut (_, ref mut count)) = self.stack.last_mut() {
            count.inc()?;
        }
        self.raw.push(kind as u8);
        self.raw.extend_from_slice(&cvt(offset_x).to_be_bytes());
        self.raw.extend_from_slice(&cvt(offset_y).to_be_bytes());
        self.raw.extend_from_slice(&cvt(width).to_be_bytes());
        self.raw.extend_from_slice(&cvt(height).to_be_bytes());
        self.raw.extend_from_slice(&[0, 0]);
        self.raw.extend_from_slice(&text_len.to_be_bytes());
        let pos = self.raw.len();
        self.raw.extend_from_slice(&[0, 0, 0]); // num_children, fixed up later
        self.stack.push((pos, U24(0)));
        Ok(())
    }

    fn end_zone(&mut self) {
        let (pos, count) = self.stack.pop().unwrap();
        self.raw[pos..pos + 3].copy_from_slice(&count.to_be_bytes());
    }
}

pub struct Txt(AddingZones);

impl Txt {
    pub fn new(text: &str) -> Result<Self, Error> {
        let mut raw = Vec::new();
        let len: U24 = text.len().try_into()?;
        raw.extend_from_slice(&len.to_be_bytes());
        raw.extend_from_slice(text.as_bytes());
        raw.extend_from_slice(&crate::TxtVersion::CURRENT.pack());
        Ok(Self(AddingZones {
            raw,
            stack: Vec::new(),
        }))
    }

    pub fn start_zone(
        &mut self,
        kind: crate::ZoneKind,
        offset_x: i16,
        offset_y: i16,
        width: i16,
        height: i16,
        text_len: U24,
    ) -> Result<(), OverflowError> {
        self.0.start_zone(kind, offset_x, offset_y, width, height, text_len)
    }

    pub fn end_zone(&mut self) {
        self.0.end_zone()
    }

    pub fn bytes(&self) -> &[u8] {
        if !self.0.stack.is_empty() {
            panic!()
        }
        &self.0.raw
    }
}

#[derive(Default)]
pub struct Zones(AddingZones);

impl Zones {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start_zone(
        &mut self,
        kind: crate::ZoneKind,
        offset_x: i16,
        offset_y: i16,
        width: i16,
        height: i16,
        text_len: U24,
    ) -> Result<(), OverflowError> {
        self.0.start_zone(kind, offset_x, offset_y, width, height, text_len)
    }

    pub fn end_zone(&mut self) {
        self.0.end_zone()
    }
}

pub struct Outline {
    raw: Vec<u8>,
    count: u16,
    stack: Vec<(usize, u8)>,
}

impl Outline {
    pub fn new() -> Self {
        Self { raw: alloc::vec![0, 0], count: 0, stack: Vec::new() }
    }

    pub fn start_bookmark(&mut self, description: &str, url: &str) -> Result<(), OverflowError> {
        let description_len: U24 = description.len().try_into()?;
        let url_len: U24 = url.len().try_into()?;
        self.count += 1;
        self.raw[0..2].copy_from_slice(&self.count.to_be_bytes());
        if let Some(&mut (_, ref mut n)) = self.stack.last_mut() {
            *n = (*n).checked_add(1).ok_or(OverflowError)?;
        }
        self.stack.push((self.raw.len(), 0));
        self.raw.push(0); // fixed up later
        self.raw.extend_from_slice(&description_len.to_be_bytes());
        self.raw.extend_from_slice(description.as_bytes());
        self.raw.extend_from_slice(&url_len.to_be_bytes());
        self.raw.extend_from_slice(url.as_bytes());
        Ok(())
    }

    pub fn end_bookmark(&mut self) {
        let (i, n) = self.stack.pop().unwrap();
        self.raw[i] = n;
    }

    pub fn bytes(&self) -> &[u8] {
        if !self.stack.is_empty() {
            panic!()
        }
        &self.raw
    }
}
