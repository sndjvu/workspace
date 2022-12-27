//! Serialization to the DjVu transfer format.
//!
//! If you have a Rust data type that represents a DjVu document, implementing this module's
//! [`Serialize`] trait allows to you turn a value of that type into a blob of bytes in the
//! transfer format. The structure of the document is expressed by a sequence of method calls on
//! various "serializer" objects, such as [`Serializer`], [`SerializeMultiPageBundled`], etc. This
//! approach should be familiar from APIs like `std::fmt::Debug` and `serde::Serialize`.
//!
//! ## Two-pass approach
//!
//! The DjVu transfer format makes extensive use of length-prefixed fields. Because DjVu documents
//! have tree structure, this creates a chicken/egg problem for serialization: we want to emit the
//! bytes of the serialized document in order, but we don't know what value to use for (e.g.) the
//! length field in the document header until the entire structure of the document has been
//! described.
//!
//! In fact, it's even worse than this: some length metatada is stored in the *compressed* portion
//! of the `DIRM` header chunk. Since the length of the compressed data is unpredictable until we
//! know exactly what data will be compressed, it's not possible to simply write junk to these
//! fields and fix them up later in serialization (which approach does work for length fields that
//! are stored plain).
//!
//! The solution is to split serialization into two passes. On the first pass, we don't emit any
//! bytes, but only collect enough information to compute the value of every length field. On the
//! second pass, we use that stored information to emit the bytes of the document, in order, with
//! their correct values.
//!
//! The trick that this module pulls off is to *encapsulate* the two-pass nature of serialization.
//! You just implement [`Serialize`], and a function like [`to_vec`] takes care internally of
//! calling [`Serialize::serialize`] twice, with two different [`Serializer`]s, one for the first
//! pass and one for the second.

#![deny(clippy::integer_arithmetic)]

use crate::{
    ComponentKind, DirmVersion, FgbzVersion, InfoVersion,
    Iw44ColorSpace, Iw44Version, PageRotation, PaletteEntry,
    PhantomMutable, TxtVersion, Zone, ZoneKind,
};
use crate::annot::Annot;
use core::fmt::{Debug, Display, Formatter};
use core::mem::replace;
use alloc::string::String;
use alloc::vec::Vec;
#[cfg(feature = "backtrace")]
use std::backtrace::Backtrace;

const CHUNK_HEADER_SIZE: u32 = 8;
const COMPONENT_HEADER_SIZE: u32 = 12;

/// Trivial error type for checked arithmetic.
#[derive(Debug)]
struct OverflowError;

impl Display for OverflowError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "arithmetic overflow while computing value for length or offset field")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for OverflowError {}

#[derive(Debug)]
enum ErrorKind {
    Overflow,
    #[cfg(feature = "std")]
    Io(std::io::Error),
}

/// An error encountered during serialization.
///
/// Contains a backtrace if the `backtrace` crate feature is enabled.
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    #[cfg(feature = "backtrace")]
    _backtrace: Backtrace,
    _mutable: PhantomMutable,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self.kind {
            ErrorKind::Overflow => write!(f, "{}", OverflowError)?,
            #[cfg(feature = "std")]
            ErrorKind::Io(ref e) => write!(f, "{e}")?,
        }
        Ok(())
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

impl Error {
    #[cfg(feature = "std")]
    fn io(e: std::io::Error) -> Self {
        Self {
            kind: ErrorKind::Io(e),
            #[cfg(feature = "backtrace")]
            _backtrace: Backtrace::capture(),
            _mutable: PhantomMutable,
        }
    }
}

impl From<OverflowError> for Error {
    fn from(_: OverflowError) -> Self {
        Self {
            kind: ErrorKind::Overflow,
            #[cfg(feature = "backtrace")]
            _backtrace: Backtrace::capture(),
            _mutable: PhantomMutable,
        }
    }
}

macro_rules! checked_sum {
    ( $( $a:expr ),* $( , )? ) => {
        (|| -> Result<u32, OverflowError> {
            let mut total = 0u32;
            $(
                total = total.checked_add($a).ok_or(OverflowError)?;
            )*
            Ok(total)
        })()
    };
}

macro_rules! tame {
    ( $( $tt:tt )* ) => {
        {
            #[allow(clippy::integer_arithmetic)]
            let x = { $($tt)* };
            x
        }
    };
}

trait Out {
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
impl<W: std::io::Write> Out for W {
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
        self.inner.next().map(|[b1, b2, b3]| tame!(u32::from_be_bytes([0, b1, b2, b3]) - CHUNK_HEADER_SIZE))
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

    fn push(&mut self, kind: ComponentKind, id: &str) {
        self.flags_buf.push(kind as u8);
        self.ids_buf.extend_from_slice(id.as_bytes());
        self.ids_buf.push(b'\0');
    }
}

/// Opaque token returned from successful serialization.
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
        let len: u32 = b.len().try_into().map_err(|_| OverflowError)?;
        self.running = checked_sum!(self.running, len)?;
        Ok(())
    }
}

enum Pass<'wr> {
    First(First),
    Second(Second<'wr>),
}

impl<'wr> Pass<'wr> {
    fn put(&mut self, b: &[u8]) -> Result<(), Error> {
        match *self {
            Pass::First(ref mut first) => {
                // in practice it seems we only use this to write chunk data,
                // which means calling it without Cur::InChunk is a bug
                // the behavior is to forward to the underlying Out and also
                // increment the chunk counter
                let len: u32 = b.len().try_into().map_err(|_| OverflowError)?;
                match first.cur {
                    Cur::InChunk { ref mut chunk, .. } => {
                        *chunk = checked_sum!(*chunk, len)?;
                    }
                    _ => unreachable!(),
                }
            }
            Pass::Second(ref mut second) => second.put(b)?,
        }
        Ok(())
    }

    fn start_component(&mut self, kind: ComponentKind, id: &str) -> Result<(), Error> {
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
                    out!(second; [0])?;
                }
                let len = second.component_lens.next().unwrap();
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
                    out!(second; [0])?;
                }
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

/// The starting point for serialization.
///
/// An implementation of [`Serialize::serialize`] should call one of the methods of this type to
/// select what kind of document is being serialized. Currently, only the bundled multi-page
/// document format is supported.
pub struct Serializer<'wr> {
    repr: SerializerRepr<'wr>,
}

/// Interface for describing the structure of a DjVu document.
///
/// Currently, only the bundled multi-page document structure is supported.
pub trait Serialize {
    fn serialize(&self, serializer: Serializer<'_>) -> Result<Okay, Error>;
}

impl<'wr> Serializer<'wr> {
    fn first_pass() -> Self {
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

    fn second_pass(okay: Okay, out: ErasedOutMut<'wr>) -> Self {
        let Okay { num_components, dirm_data, chunk_lens, component_sizes, total } = okay;
        let _ = total; // not used
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
                SerializeComponents { pass: Pass::First(first) },
            ),
            SerializerRepr::Second(compress_dirm) => SerializeMultiPageBundled::Head(compress_dirm),
        }
    }

    // TODO other document formats?
}

/// Serializer for a bundled multi-page document.
///
/// The two variants of this type expose the two-pass nature of the serialization process. On the
/// first pass, an implementation of [`Serialize::serialize`] will be presented with the
/// `Components` variant and proceed directly to describing the components of the document. On the
/// second pass, after enough information has been gathered to compute the contents of the `DIRM`
/// chunk, the implementation is presented with the `Head` variant and must compress (a portion of)
/// the `DIRM` data and pass it back to the serializer.
pub enum SerializeMultiPageBundled<'wr> {
    Components(SerializeComponents<'wr>),
    Head(SerializeMultiPageHead<'wr>),
}

/// Serializer for the "head" data of a multi-page document (`DIRM` and `NAVM` chunks).
pub struct SerializeMultiPageHead<'wr> {
    num_components: u16,
    dirm_data: Vec<u8>,
    chunk_lens: Vec<u32>,
    component_sizes: ComponentSizes,
    out: ErasedOutMut<'wr>,
}

impl<'wr> SerializeMultiPageHead<'wr> {
    /// Access raw data for BZZ compression.
    ///
    /// The returned bytes should be compressed using a BZZ implementation, and the compressed data
    /// passed as the first argument to [`SerializeMultiPageHead::dirm_and_navm`] to continue
    /// serialization.
    pub fn for_compression(&self) -> &[u8] {
        &self.dirm_data
    }

    /// Provide compressed `DIRM` and `NAVM` data to continue with serialization.
    ///
    /// Create the `NAVM` data by building up a [`BookmarkBuf`] and passing the output of
    /// [`BookmarkBuf::as_bytes`] to a BZZ compressor.
    pub fn dirm_and_navm(self, compressed: &[u8], navm: Option<&[u8]>) -> Result<SerializeComponents<'wr>, Error> {
        // accumulate offsets
        let mut off: u32 = tame!(4 + COMPONENT_HEADER_SIZE); // AT&T:FORM:$len:DJVM
        tame!(off += CHUNK_HEADER_SIZE); // DIRM:$len
        let mut dirm_len = tame!(1 + 2); // $flags:$num_components
        let addl = 4u32.checked_mul(self.num_components as u32).ok_or(OverflowError)?;
        dirm_len = checked_sum!(dirm_len, addl)?;
        let addl: u32 = compressed.len().try_into().map_err(|_| OverflowError)?;
        dirm_len = checked_sum!(dirm_len, addl)?;
        off = checked_sum!(off, dirm_len)?;
        let navm_with_len = if let Some(navm) = navm {
            Some((navm, navm.len().try_into().map_err(|_| OverflowError)?))
        } else {
            None
        };
        if let Some((_, navm_len)) = navm_with_len {
            if off % 2 != 0 {
                off = checked_sum!(off, 1)?;
            }
            off = checked_sum!(off, CHUNK_HEADER_SIZE, navm_len)?;
        }
        let running = off; // save for later
        let mut offsets = Vec::new();
        for size in self.component_sizes.iter() {
            if off % 2 != 0 {
                off = checked_sum!(off, 1)?;
            }
            offsets.push(off.to_be_bytes());
            off = checked_sum!(off, size)?;
        }
        let full_len = tame!(off - 12);
        
        out!(
            self.out;
            b"AT&T", b"FORM", full_len.to_be_bytes(), b"DJVM",
            b"DIRM", dirm_len.to_be_bytes(),
            DirmVersion::CURRENT.pack(true), self.num_components.to_be_bytes(),
            crate::shim::arrays_as_slice(&offsets),
            compressed,
        )?;
        if let Some((navm, navm_len)) = navm_with_len {
            if dirm_len % 2 != 0 {
                out!(self.out; [0])?;
            }
            out!(self.out; b"NAVM", navm_len.to_be_bytes(), navm)?;
        }

        Ok(SerializeComponents {
            pass: Pass::Second(Second {
                chunk_lens: self.chunk_lens.into_iter(),
                component_lens: self.component_sizes.into_lengths(),
                running,
                out: self.out,
            })
        })
    }
}

/// Serializer for the components of a multi-page document.
pub struct SerializeComponents<'wr> {
    pass: Pass<'wr>,
}

impl<'wr> SerializeComponents<'wr> {
    /// Begin serializing a `DJVI` component.
    pub fn djvi(&mut self, id: &str) -> Result<SerializeElements<'_, 'wr>, Error> {
        self.pass.start_component(ComponentKind::Djvi, id)?;
        Ok(SerializeElements { pass: &mut self.pass })
    }

    /// Begin serializing a `DJVU` component.
    pub fn djvu(
        &mut self,
        id: &str,
        width: u16,
        height: u16,
        dpi: u16,
        gamma: u8,
        rotation: PageRotation,
    ) -> Result<SerializeElements<'_, 'wr>, Error> {
        self.pass.start_component(ComponentKind::Djvu, id)?;
        self.pass.start_chunk(b"INFO")?;
        out!(
            self.pass;
            width.to_be_bytes(),
            height.to_be_bytes(),
            InfoVersion::CURRENT.pack(),
            dpi.to_le_bytes(),
            [gamma],
            [rotation as u8],
        )?;
        Ok(SerializeElements { pass: &mut self.pass })
    }

    /// Begin serializing a `THUM` component.
    pub fn thum(&mut self, id: &str) -> Result<SerializeThumbnails<'_, 'wr>, Error> {
        self.pass.start_component(ComponentKind::Thum, id)?;
        Ok(SerializeThumbnails { pass: &mut self.pass })
    }

    /// Finish serialization after all components have been described.
    pub fn finish(self) -> Result<Okay, Error> {
        match self.pass {
            Pass::First(First {
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
                let mut dirm_data = Vec::with_capacity(tame!(
                    component_sizes.as_bytes().len() +
                    component_meta.flags_buf.len() +
                    component_meta.ids_buf.len()
                ));
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
            Pass::Second(_) => Ok(Okay::dummy()),
        }
    }
}

/// Serializer for the elements of a `DJVU` or `DJVI` component.
pub struct SerializeElements<'co, 'wr: 'co> {
    pass: &'co mut Pass<'wr>,
}

impl<'co, 'wr: 'co> SerializeElements<'co, 'wr> {
    /// Serialize an `ANTa` chunk.
    ///
    /// The DjVu standard notes that "the use of the `ANTa` chunk is discouraged", the compressed
    /// `ANTz` chunk being preferred (see [`Self::antz`]).
    ///
    /// See [`AnnotBuf`] for a strongly-typed way to build up data for the `ant` argument.
    pub fn anta(&mut self, ant: &str) -> Result<(), Error> {
        self.pass.start_chunk(b"ANTa")?;
        out!(self.pass; ant.as_bytes())?;
        Ok(())
    }

    /// Serialize an `ANTz` chunk.
    ///
    /// See [`AnnotBuf`] for a strongly-typed way to build up data that can be compressed for the
    /// `bzz` argument.
    pub fn antz(&mut self, bzz: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"ANTz")?;
        out!(self.pass; bzz)?;
        Ok(())
    }

    /// Serialize a `TXTa` chunk.
    ///
    /// The DjVu standard notes that "the use of the `TXTa` chunk is discouraged", the compressed
    /// `TXTz` chunk being preferred (see [`Self::txtz`]).
    ///
    /// See [`ZoneBuf`] for a strongly-typed way to build up data for the `zones` argument.
    pub fn txta(&mut self, text: &str, zones: &[Zone]) -> Result<(), Error> {
        let len: U24 = text.len().try_into()?;
        self.pass.start_chunk(b"TXTa")?;
        out!(
            self.pass;
            len.to_be_bytes(),
            text,
            TxtVersion::CURRENT.pack(),
            crate::shim::arrays_as_slice(Zone::uncast_slice(zones)),
        )?;
        Ok(())
    }

    /// Serialize a `TXTz` chunk.
    ///
    /// See [`TxtBuf`] for a strongly-typed way to build up data that can be compressed for the
    /// `bzz` argument.
    pub fn txtz(&mut self, bzz: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"TXTz")?;
        out!(self.pass; bzz)?;
        Ok(())
    }

    /// Serialize a `Djbz` chunk.
    pub fn djbz(&mut self, jb2: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"Djbz")?;
        out!(self.pass; jb2)?;
        Ok(())
    }

    /// Serialize an `Sjbz` chunk.
    pub fn sjbz(&mut self, jb2: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"Sjbz")?;
        out!(self.pass; jb2)?;
        Ok(())
    }

    /// Serialize an `FG44` chunk.
    ///
    /// `initial_cdc` must not exceed 127.
    pub fn fg44(
        &mut self,
        num_slices: u8,
        color_space: Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: u8,
        iw44: &[u8],
    ) -> Result<(), Error> {
        if initial_cdc > 0x7f {
            panic!()
        }
        self.pass.start_chunk(b"FG44")?;
        out!(
            self.pass;
            [0], // serial number
            [num_slices],
            Iw44Version::CURRENT.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc],
            iw44,
        )?;
        Ok(())
    }

    /// Begin serializing a sequence of `BG44` chunks.
    ///
    /// `initial_cdc` must not exceed 127.
    pub fn bg44(
        &mut self,
        num_slices: u8,
        color_space: Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: u8,
        iw44: &[u8],
    ) -> Result<SerializeBg44Chunks<'_, 'wr>, Error> {
        if initial_cdc > 0x7f {
            panic!()
        }
        self.pass.start_chunk(b"BG44")?;
        out!(
            self.pass;
            [0], // serial number
            [num_slices],
            Iw44Version::CURRENT.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc],
            iw44,
        )?;
        Ok(SerializeBg44Chunks {
            serial: 1,
            pass: &mut self.pass,
        })
    }

    /// Serialize an `FGbz` chunk.
    ///
    /// If `indices` are provided, the first element of the tuple should be the number of indices,
    /// and the second element should be the BZZ-compressed indices (pass the output of
    /// [`crate::PaletteIndex::slice_as_bytes`] to a BZZ compressor).
    pub fn fgbz(&mut self, palette: &[PaletteEntry], indices: Option<(usize, &[u8])>) -> Result<(), Error> {
        let palette_len: U24 = palette.len().try_into()?;
        out!(
            self.pass;
            FgbzVersion::CURRENT.pack(indices.is_some()),
            palette_len.to_be_bytes(),
            crate::shim::arrays_as_slice(PaletteEntry::uncast_slice(palette)),
        )?;
        if let Some((len, bzz)) = indices {
            let len: U24 = len.try_into()?;
            out!(
                self.pass;
                len.to_be_bytes(),
                bzz,
            )?;
        }
        Ok(())
    }

    /// Serialize an `INCL` chunk.
    pub fn incl(&mut self, target_id: &str) -> Result<(), Error> {
        self.pass.start_chunk(b"INCL")?;
        out!(self.pass; target_id.as_bytes())?;
        Ok(())
    }

    /// Serialize a `BGjp` chunk.
    pub fn bgjp(&mut self, jpeg: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"BGjp")?;
        out!(self.pass; jpeg)?;
        Ok(())
    }

    /// Serialize an `FGjp` chunk.
    pub fn fgjp(&mut self, jpeg: &[u8]) -> Result<(), Error> {
        self.pass.start_chunk(b"FGjp")?;
        out!(self.pass; jpeg)?;
        Ok(())
    }

    // TODO Smmr
    // TODO copy from crate::parsing::Element
}

/// Serializer for a sequence of `BG44` chunks within a `DJVI` or `DJVU` component.
pub struct SerializeBg44Chunks<'el, 'wr: 'el> {
    serial: u8,
    pass: &'el mut Pass<'wr>,
}

impl<'el, 'wr: 'el> SerializeBg44Chunks<'el, 'wr> {
    pub fn chunk(
        &mut self,
        num_slices: u8,
        iw44: &[u8],
    ) -> Result<(), Error> {
        self.pass.start_chunk(b"BG44")?;
        out!(
            self.pass;
            [self.serial],
            [num_slices],
            iw44,
        )?;
        self.serial = self.serial.checked_add(1).ok_or(OverflowError)?;
        Ok(())
    }
}

/// Serializer for the sequence of `TH44` chunks within a `THUM` component.
pub struct SerializeThumbnails<'co, 'wr: 'co> {
    pass: &'co mut Pass<'wr>,
}

impl<'co, 'wr: 'co> SerializeThumbnails<'co, 'wr> {
    pub fn th44(
        &mut self,
        num_slices: u8,
        color_space: Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: u8,
        iw44: &[u8],
    ) -> Result<(), Error> {
        if initial_cdc > 0x7f {
            return Err(OverflowError.into());
        }
        self.pass.start_chunk(b"TH44")?;
        let version = Iw44Version::CURRENT;
        out!(
            self.pass;
            [0], // serial number
            [num_slices],
            version.pack(color_space),
            width.to_be_bytes(),
            height.to_be_bytes(),
            [initial_cdc],
            iw44,
        )?;
        Ok(())
    }
}

/// Serialize a document and write the serialized data to the given sink.
///
/// The serializer makes many small writes, so make sure that `writer` is buffered.
#[cfg(feature = "std")]
pub fn to_writer<T: Serialize, W: std::io::Write>(doc: &T, mut writer: W) -> Result<(), Error> {
    let serializer = Serializer::first_pass();
    let okay = doc.serialize(serializer)?;
    let serializer = Serializer::second_pass(okay, &mut writer as _);
    let _okay = doc.serialize(serializer)?;
    Ok(())
}

/// Serialize a document and return a buffer of serialized data.
pub fn to_vec<T: Serialize>(doc: &T) -> Result<Vec<u8>, Error> {
    let serializer = Serializer::first_pass();
    let okay = doc.serialize(serializer)?;
    let mut buf = VecOut(Vec::with_capacity(okay.total as _));
    let serializer = Serializer::second_pass(okay, &mut buf as _);
    let _okay = doc.serialize(serializer)?;
    Ok(buf.0)
}

struct U24(u32);

impl U24 {
    fn to_be_bytes(self) -> [u8; 3] {
        let [_, b1, b2, b3] = self.0.to_be_bytes();
        [b1, b2, b3]
    }

    fn inc(&mut self) -> Result<(), OverflowError> {
        if tame!(self.0 + 1 < 1 << 24) {
            tame!(self.0 += 1);
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

/// Builder for the (uncompressed) data in an `ANTa` or `ANTz` chunk.
pub struct AnnotBuf {
    raw: String,
}

impl AnnotBuf {
    pub fn new() -> Self {
        Self { raw: String::new() }
    }

    /// Add an annotation.
    pub fn add(&mut self, annot: &Annot) {
        use core::fmt::Write;
        if self.raw.is_empty() {
            let _ = write!(self.raw, "{annot}");
        } else {
            let _ = write!(self.raw, " {annot}");
        }
    }

    pub fn as_str(&self) -> &str {
        &self.raw
    }
}

impl Zone {
    fn new(
        kind: ZoneKind,
        x_offset: i16,
        y_offset: i16,
        width: i16,
        height: i16,
        text_len: U24,
        num_children: U24,
    ) -> Self {
        fn cvt(x: i16) -> u16 {
            tame!((x as u16) ^ (1 << 15))
        }

        Zone {
            kind,
            x_offset: cvt(x_offset).to_be_bytes(),
            y_offset: cvt(y_offset).to_be_bytes(),
            width: cvt(width).to_be_bytes(),
            height: cvt(height).to_be_bytes(),
            _empty: [0, 0],
            text_len: text_len.to_be_bytes(),
            num_children: num_children.to_be_bytes(),
        }
    }
}

/// Builder for the (uncompressed) data in a `TXTz` chunk.
///
/// Calls to [`Self::start_zone`] and [`Self::end_zone`] must be balanced, and their nesting
/// determines the tree structure of the zones in the natural way.
pub struct TxtBuf {
    raw: Vec<u8>,
    stack: Vec<(usize, U24)>,
}

impl TxtBuf {
    pub fn new(text: &str) -> Result<Self, Error> {
        let mut raw = Vec::new();
        let len: U24 = text.len().try_into()?;
        raw.extend_from_slice(&len.to_be_bytes());
        raw.extend_from_slice(text.as_bytes());
        raw.extend_from_slice(&TxtVersion::CURRENT.pack());
        Ok(Self {
            raw,
            stack: Vec::new(),
        })
    }

    pub fn start_zone(
        &mut self,
        kind: ZoneKind,
        x_offset: i16,
        y_offset: i16,
        width: i16,
        height: i16,
        text_len: usize,
    ) -> Result<(), Error> {
        let text_len: U24 = text_len.try_into()?;
        if let Some(&mut (_, ref mut count)) = self.stack.last_mut() {
            count.inc()?;
        }
        let pos = self.raw.len() + 14;
        let zone = Zone::new(
            kind,
            x_offset,
            y_offset,
            width,
            height,
            text_len,
            U24(0), // fixed up later
        );
        self.raw.extend_from_slice(zone.as_bytes());
        self.stack.push((pos, U24(0)));
        Ok(())
    }

    pub fn end_zone(&mut self) {
        let (pos, count) = self.stack.pop().unwrap();
        self.raw[pos..tame!(pos + 3)].copy_from_slice(&count.to_be_bytes());
    }

    pub fn as_bytes(&self) -> &[u8] {
        if !self.stack.is_empty() {
            panic!()
        }
        &self.raw
    }
}

/// Builder for the "zones" data in a `TXTa` chunk.
///
/// Calls to [`Self::start_zone`] and [`Self::end_zone`] must be balanced, and their nesting
/// determines the tree structure of the zones in the natural way.
#[derive(Default)]
pub struct ZoneBuf {
    stack: Vec<(usize, U24)>,
    inner: Vec<Zone>,
}

impl ZoneBuf {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start_zone(
        &mut self,
        kind: ZoneKind,
        x_offset: i16,
        y_offset: i16,
        width: i16,
        height: i16,
        text_len: usize,
    ) -> Result<(), Error> {
        let text_len = text_len.try_into()?;
        if let Some(&mut (_, ref mut n)) = self.stack.last_mut() {
            n.inc()?;
        }
        let pos = self.inner.len();
        let zone = Zone::new(
            kind,
            x_offset,
            y_offset,
            width,
            height,
            text_len,
            U24(0), // fixed up later
        );
        self.inner.push(zone);
        self.stack.push((pos, U24(0)));
        Ok(())
    }

    pub fn end_zone(&mut self) {
        let (i, n) = self.stack.pop().unwrap();
        self.inner[i].num_children = n.to_be_bytes();
    }

    pub fn as_zones(&self) -> &[Zone] {
        &self.inner
    }
}

/// Builder for the uncompressed contents of a `NAVM` chunk.
///
/// Calls to [`Self::start_bookmark`] and [`Self::end_bookmark`] must be balanced, and their nesting
/// determines the tree structure of the bookmarks in the natural way.
pub struct BookmarkBuf {
    raw: Vec<u8>,
    count: u16,
    stack: Vec<(usize, u8)>,
}

impl BookmarkBuf {
    pub fn new() -> Self {
        Self { raw: alloc::vec![0, 0], count: 0, stack: Vec::new() }
    }

    pub fn start_bookmark(&mut self, description: &str, url: &str) -> Result<(), Error> {
        let description_len: U24 = description.len().try_into()?;
        let url_len: U24 = url.len().try_into()?;
        self.count = self.count.checked_add(1).ok_or(OverflowError)?;
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

    pub fn as_bytes(&self) -> &[u8] {
        if !self.stack.is_empty() {
            panic!()
        }
        &self.raw
    }
}
