//! Low-level parser for the DjVu transfer format.
//!
//! ## Rundown
//!
//! ### Entry points
//!
//! - [`document`]
//! - [`indirect_component`]
//!
//! ### Pointer types
//!
//! - [`ComponentP`]
//! - [`ElementP`]
//! - [`ThumbnailP`]
//!
//! ### Raw chunks
//!
//! ### Parsed chunks
//!
//! ## More on "pointers"
//!
//! Here's an illustration of how to think about [`ComponentP`] and [`ElementP`]
//! ([`ThumbnailP`] is completely analogous):
//!
//! ```text
//!                                       ElementP  ElementP  ElementP  ElementP
//!                                       v         v         v         v
//! +-----------+------+-----------+------+---------+---------+---------+
//! | FORM:DJVM | DIRM | FORM:DJVU | INFO | element | element | element |
//! +-----------+------+-----------+------+---------+---------+---------+
//!                    ^                                                ^
//!                    ComponentP                                       ComponentP
//! ```
//!
//! Of note, all these types can validly be "one-past-the-end". This extends the
//! analogy with pointers and is necessary anyway to support documents with no
//! components and components with no elements or thumbnails. Trying to `feed`
//! a one-past-the-end value will always return [`Progress::End`].

use crate::Bstr;
use core::fmt::{Debug, Formatter};
use core::num::NonZeroU8;
use alloc::vec::Vec;
#[cfg(sndjvu_backtrace)]
use std::backtrace::Backtrace;

/// The outcome of a parsing operation, if no [`Error`] was encountered.
pub enum Progress<T, D = Void> {
    /// Not enough data was presented to complete the parsing operation.
    None {
        /// Significance to be determined.
        hint: Option<usize>,
    },
    /// Parsing was successful.
    Advanced {
        /// The parsed object.
        head: T,
        /// How many bytes were consumed.
        ///
        /// The caller should advance its byte stream by this amount.
        by: usize,
    },
    /// Nothing remains to be parsed.
    ///
    /// Note that if `D` is uninhabited (see [`Void`]) than this variant is impossible to construct, and can
    /// essentially be ignored when matching.
    End(D),
}

/// Uninhabited type, used to punch a hole in [`Progress`].
///
/// This will become an alias for the never type [`!`] once that's stabilized. In the meantime,
/// `Void` does not enjoy the same special coercions as `!`, but you can mimic them like this:
///
/// ```
/// fn explode<T>(x: Void) -> T {
///     match x {}
/// }
/// ```
pub enum Void {}

fn advanced<T, D>(head: T, s: &SplitOuter<'_>) -> Progress<T, D> {
    Progress::Advanced { head, by: s.by as usize }
}

enum ProgressInternal<T> {
    None(Option<usize>),
    Advanced(T),
}

macro_rules! try_advance {
    ( $x:expr ) => {
        match $x {
            ProgressInternal::None(hint) => return Ok(Progress::None { hint }),
            ProgressInternal::Advanced(head) => head,
        }
    };
}

#[derive(Debug)]
pub struct Error {
    #[cfg(sndjvu_backtrace)]
    backtrace: Backtrace,
}

impl core::fmt::Display for Error {
    fn fmt(&self, _f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        #[cfg(sndjvu_backtrace)]
        { write!(_f, "{}", self.backtrace)?; }
        Ok(())
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

fn error_placeholder() -> Error {
    Error {
        #[cfg(sndjvu_backtrace)]
        backtrace: Backtrace::capture(),
    }
}

pub fn document(data: &[u8]) -> Result<Progress<DocumentHead<'_>>, Error> {
    let mut s = split_outer(data, 0, None); // don't know end_pos yet
    let s = &mut s;
    let (kind, len) = try_advance!(s.magic_form_header()?);
    let end_pos = s.set_distance_to_end(len);
    let head = match &kind {
        b"DJVU" => {
            let content = try_advance!(s.specific_chunk(b"INFO")?);
            let info = RawInfo { content };
            let elements = ElementP::new(s.pos(), end_pos);
            DocumentHead::SinglePage { info, elements }
        }
        b"DJVM" => {
            let content = try_advance!(s.specific_chunk(b"DIRM")?);
            let dirm = RawDirm { content, end_pos };
            let kind = try_advance!(s.peek_chunk()?);
            let navm = if &kind == b"NAVM" {
                let content = try_advance!(s.specific_chunk(b"NAVM")?);
                Some(RawNavm { content })
            } else {
                None
            };
            DocumentHead::MultiPage { dirm, navm }

        }
        _ => return Err(error_placeholder()),
    };
    Ok(advanced(head, s))
}

pub fn indirect_component(data: &[u8]) -> Result<Progress<ComponentHead<'_>>, Error> {
    let mut s = split_outer(data, 0, None);
    let s = &mut s;
    let (kind, len) = try_advance!(s.magic_form_header()?);
    let head = try_advance!(s.component_head(kind, len)?);
    Ok(advanced(head, s))
}


/// Parsed representation of the start of a document.
#[derive(Clone, Debug)]
pub enum DocumentHead<'a> {
    SinglePage {
        info: RawInfo<'a>,
        elements: ElementP,
    },
    MultiPage {
        dirm: RawDirm<'a>,
        navm: Option<RawNavm<'a>>,
    },
}

#[derive(Clone, Debug)]
pub struct RawInfo<'a> {
    content: Field<'a>,
}

impl<'a> RawInfo<'a> {
    pub fn parse(&self) -> Result<Info, Error> {
        let mut s = self.content.split();
        let width = s.u16_be()?;
        let height = s.u16_be()?;
        let &[minor, major] = s.array()?;
        let dpi = s.u16_le()?;
        let gamma = s.byte()?;
        let flags = s.byte()?;
        let rotation = match flags & 0b111 {
            1 => crate::PageRotation::Up,
            6 => crate::PageRotation::Ccw,
            2 => crate::PageRotation::Down,
            5 => crate::PageRotation::Cw,
            _ => crate::PageRotation::Up, // see djvuchanges.txt
        };
        Ok(Info {
            width,
            height,
            version: crate::InfoVersion { major, minor },
            dpi,
            gamma,
            rotation,
        })
    }
}

pub struct Info {
    pub width: u16,
    pub height: u16,
    pub version: crate::InfoVersion,
    pub dpi: u16,
    pub gamma: u8,
    pub rotation: crate::PageRotation,
}

fn is_potential_chunk_id(xs: [u8; 4]) -> bool {
    xs.iter().all(u8::is_ascii_alphanumeric)
}

/// Parsed representation of an element of a page.
pub enum Element<'a> {
    Anta(RawAnta<'a>),
    Antz(RawAntz<'a>),
    Txta(RawTxta<'a>),
    Txtz(RawTxtz<'a>),
    Djbz(RawDjbz<'a>),
    Sjbz(RawSjbz<'a>),
    Fg44(RawFg44<'a>),
    Bg44(RawBg44<'a>),
    Fgbz(RawFgbz<'a>),
    Incl(RawIncl<'a>),
    Bgjp(RawBgjp<'a>),
    Fgjp(RawFgjp<'a>),
    Smmr(RawSmmr<'a>),
    Unknown(Chunk<'a>),
}

impl<'a> Element<'a> {
    pub fn after(&self) -> ElementP {
        match *self {
            Self::Anta(RawAnta { after_pos, end_pos, .. })
            | Self::Antz(RawAntz { after_pos, end_pos, .. })
            | Self::Txta(RawTxta { after_pos, end_pos, .. })
            | Self::Txtz(RawTxtz { after_pos, end_pos, .. })
            | Self::Djbz(RawDjbz { after_pos, end_pos, .. })
            | Self::Sjbz(RawSjbz { after_pos, end_pos, .. })
            | Self::Fg44(RawFg44 { after_pos, end_pos, .. })
            | Self::Bg44(RawBg44 { after_pos, end_pos, .. })
            | Self::Fgbz(RawFgbz { after_pos, end_pos, .. })
            | Self::Incl(RawIncl { after_pos, end_pos, .. })
            | Self::Bgjp(RawBgjp { after_pos, end_pos, .. })
            | Self::Fgjp(RawFgjp { after_pos, end_pos, .. })
            | Self::Smmr(RawSmmr { after_pos, end_pos, .. })
            | Self::Unknown(Chunk { after_pos, end_pos, .. })
                => ElementP::new(after_pos, end_pos),
        }
    }
}

pub struct RawAnta<'a> {
    content: StringField<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawAnta<'a> {
    pub fn parsing(&self) -> ParsingAnnots<'a> {
        ParsingAnnots::new(self.content)
    }
}

pub struct RawAntz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawAntz<'a> {
    pub fn bzz(&self) -> &'a [u8] {
        self.content.bytes()
    }

    pub fn decoded<'dec>(&self, decoded: &'dec [u8]) -> DecodedAntz<'dec> {
        DecodedAntz { content: StringField(self.content.decoded(decoded)) }
    }
}

pub struct DecodedAntz<'a> {
    content: StringField<'a>,
}

impl<'a> DecodedAntz<'a> {
    pub fn parsing(&self) -> ParsingAnnots<'a> {
        ParsingAnnots::new(self.content)
    }
}

pub struct RawTxta<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawTxta<'a> {
    pub fn parse(&self) -> Result<Txt<'a>, Error> {
        Txt::parse_from(self.content.split())
    }
}

pub struct RawTxtz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawTxtz<'a> {
    pub fn bzz(&self) -> &'a [u8] {
        self.content.bytes()
    }

    pub fn parse_decoded<'dec>(&self, decoded: &'dec [u8]) -> Result<Txt<'dec>, Error> {
        Txt::parse_from(self.content.decoded(decoded).split())
    }
}

pub struct Txt<'a> {
    pub text: &'a [u8],
    pub version: crate::TxtVersion,
    pub zones: &'a [Zone],
}

impl<'a> Debug for Txt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Txt")
            .field("text", &Bstr(self.text))
            .field("version", &self.version)
            .field("zones", &self.zones)
            .finish()
    }
}

impl<'a> Txt<'a> {
    fn parse_from(mut s: SplitInner<'a>) -> Result<Self, Error> {
        let len = s.u24_be()?;
        let text = s.slice(len as usize)?;
        let version = s.byte().map(crate::TxtVersion)?;
        let raw_zones = s.rest_arrays()?;
        let zones = Zone::cast_slice(raw_zones)?;
        Ok(Txt {
            text,
            version,
            zones,
        })
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Zone {
    kind: crate::ZoneKind,
    x_offset: [u8; 2],
    y_offset: [u8; 2],
    width: [u8; 2],
    height: [u8; 2],
    _empty: [u8; 2],
    text_len: [u8; 3],
    num_children: [u8; 3],
}

fn cvt_zone_i16(bytes: [u8; 2]) -> i16 {
    i16::from_be_bytes(bytes) ^ (1 << 15)
}

impl Zone {
    fn cast_slice(arrays: &[[u8; 17]]) -> Result<&[Self], Error> {
        for arr in arrays {
            let p = arr as *const _ as *const Zone;
            let x = unsafe { *core::ptr::addr_of!((*p).kind).cast::<u8>() };
            if !(1..=7).contains(&x) {
                return Err(error_placeholder());
            }
        }

        // SAFETY Zone has the same layout as [u8; 17], with no padding,
        // and we've checked that the kind fields have valid values
        let zones = unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        };
        Ok(zones)
    }

    pub fn kind(self) -> crate::ZoneKind {
        self.kind
    }

    pub fn x_offset(self) -> i16 {
        cvt_zone_i16(self.x_offset)
    }

    pub fn y_offset(self) -> i16 {
        cvt_zone_i16(self.y_offset)
    }

    pub fn width(self) -> i16 {
        cvt_zone_i16(self.width)
    }

    pub fn height(self) -> i16 {
        cvt_zone_i16(self.height)
    }

    pub fn text_len(self) -> u32 {
        let [b1, b2, b3] = self.text_len;
        u32::from_be_bytes([0, b1, b2, b3])
    }

    pub fn num_children(self) -> u32 {
        let [b1, b2, b3] = self.num_children;
        u32::from_be_bytes([0, b1, b2, b3])
    }
}

pub struct RawDjbz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawDjbz<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.bytes()
    }
}

pub struct RawSjbz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawSjbz<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.bytes()
    }
}

pub struct RawFg44<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawFg44<'a> {
    pub fn parse(&self) -> Result<Iw44<'a>, Error> {
        Iw44::parse_from(self.content.split())
    }
}

pub struct RawBg44<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawBg44<'a> {
    pub fn parse(&self) -> Result<Iw44<'a>, Error> {
        Iw44::parse_from(self.content.split())
    }
}

pub struct Iw44<'a> {
    pub kind: Iw44Kind,
    pub num_slices: u8,
    pub body: &'a [u8],
}

impl<'a> Iw44<'a> {
    fn parse_from(mut s: SplitInner<'a>) -> Result<Self, Error> {
        let byte = s.byte()?;
        let num_slices = s.byte()?;
        let kind = if let Some(serial) = NonZeroU8::new(byte) {
            Iw44Kind::Tail { serial }
        } else {
            let byte = s.byte()?;
            let colors = if byte & (1 << 7) != 0 {
                crate::Iw44ColorSpace::Gray
            } else {
                crate::Iw44ColorSpace::YCbCr
            };
            let major = byte & 0b0111_1111;
            let minor = s.byte()?;
            let version = crate::Iw44Version { major, minor };
            let width = s.u16_be()?;
            let height = s.u16_be()?;
            let initial_cdc = crate::Cdc(s.byte()?);
            Iw44Kind::Head {
                version,
                colors,
                width,
                height,
                initial_cdc,
            }
        };
        let body = s.rest();
        Ok(Iw44 {
            kind,
            num_slices,
            body: body.bytes(),
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Iw44Kind {
    Head {
        version: crate::Iw44Version,
        colors: crate::Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: crate::Cdc,
    },
    Tail { serial: NonZeroU8 },
}

pub struct RawFgbz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawFgbz<'a> {
    pub fn parse(&self) -> Result<Fgbz<'a>, Error> {
        let mut s = self.content.split();
        let byte = s.byte()?;
        let has_indices = byte & (1 << 7) != 0;
        let version = crate::FgbzVersion(byte & 0b0111_1111);
        let num_entries = s.u16_be()?;
        let raw_palette = s.slice_of_arrays(num_entries as usize)?;
        let palette = PaletteEntry::cast_slice(raw_palette);
        let indices = if has_indices {
            Some(FgbzIndices { content: s.rest() })
        } else {
            None
        };
        Ok(Fgbz {
            version,
            palette,
            indices,
        })
    }
}

pub struct Fgbz<'a> {
    pub version: crate::FgbzVersion,
    pub palette: &'a [PaletteEntry],
    pub indices: Option<FgbzIndices<'a>>,
}

pub struct FgbzIndices<'a> {
    content: Field<'a>,
}

impl<'a> FgbzIndices<'a> {
    pub fn bzz(&self) -> &'a [u8] {
        self.content.bytes()
    }

    pub fn parse_decoded<'dec>(&self, decoded: &'dec [u8]) -> Result<&'dec [PaletteIndex], Error> {
        let mut s = self.content.decoded(decoded).split();
        let num_indices = s.u24_be()?;
        let raw_indices = s.slice_of_arrays(num_indices as usize)?;
        let indices = PaletteIndex::cast_slice(raw_indices);
        Ok(indices)
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct PaletteEntry {
    pub b: u8,
    pub g: u8,
    pub r: u8,
}

impl PaletteEntry {
    fn cast_slice(arrays: &[[u8; 3]]) -> &[Self] {
        // SAFETY PaletteEntry is repr(C) with the same layout as [u8; 3]
        unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct PaletteIndex([u8; 2]);

impl PaletteIndex {
    fn cast_slice(arrays: &[[u8; 2]]) -> &[Self] {
        // SAFETY PaletteIndex is repr(transparent)
        unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        }
    }
}

pub struct RawIncl<'a> {
    content: StringField<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawIncl<'a> {
    pub fn target_id(&self) -> &'a [u8] {
        self.content.0.bytes()
    }
}

pub struct RawBgjp<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawBgjp<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.bytes()
    }
}

pub struct RawFgjp<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawFgjp<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.bytes()
    }
}

pub struct RawSmmr<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Chunk<'a> {
    kind: Bstr<[u8; 4]>,
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> Chunk<'a> {
    pub fn kind(&self) -> [u8; 4] {
        self.kind.0
    }

    pub fn content(&self) -> &'a [u8] {
        self.content.bytes()
    }
}

#[derive(Clone, Debug)]
pub struct RawDirm<'a> {
    content: Field<'a>,
    end_pos: Pos,
}

impl<'a> RawDirm<'a> {
    pub fn parse(&self) -> Result<Dirm<'a>, Error> {
        let mut s = self.content.split();
        let flags = s.byte()?;
        let is_bundled = flags & (1 << 7) != 0;
        let version = crate::DirmVersion(flags & 0b0111_1111);
        let num_components = s.u16_be()?;
        let bundled = if is_bundled {
            let arrays = s.slice_of_arrays(num_components as usize)?;
            let offsets = ComponentOffset::cast_slice(arrays);
            Some(Bundled { offsets, end_pos: self.end_pos })
        } else {
            None
        };
        let rest = s.rest();
        Ok(Dirm {
            version,
            num_components,
            bundled,
            extra: DirmExtra { num_components, content: rest },
        })
    }
}

pub struct Dirm<'a> {
    pub version: crate::DirmVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'a>>,
    pub extra: DirmExtra<'a>,
}

impl<'a> Dirm<'a> {
    pub fn version(&self) -> crate::DirmVersion {
        self.version
    }

    pub fn num_components(&self) -> u16 {
        self.num_components
    }

    pub fn bundled(&self) -> Option<&Bundled<'a>> {
        self.bundled.as_ref()
    }
}

pub struct DirmExtra<'a> {
    num_components: u16,
    content: Field<'a>,
}

impl<'a> DirmExtra<'a> {
    pub fn bzz(&self) -> &'a [u8] {
        self.content.bytes()
    }

    pub fn parse_decoded<'dec>(&self, decoded: &'dec [u8]) -> Result<Vec<ComponentMeta<'dec>>, Error> {
        let mut s = self.content.decoded(decoded).split();
        let init_meta = ComponentMeta {
            len: 0,
            kind: crate::ComponentKind::Djvi,
            id: b"",
            name: None,
            title: None,
        };
        let mut meta = alloc::vec![init_meta; self.num_components as usize];

        for entry in &mut meta {
            entry.len = s.u24_be()?;
        }
        for entry in &mut meta {
            let flags = s.byte()?;
            if flags & (1 << 7) != 0 {
                entry.name = Some(b"");
            }
            if flags & (1 << 6) != 0 {
                entry.title = Some(b"");
            }
            entry.kind = match flags & 0b0011_1111 {
                0 => crate::ComponentKind::Djvi,
                1 => crate::ComponentKind::Djvu,
                2 => crate::ComponentKind::Thum,
                _ => return Err(error_placeholder()),
            }
        }
        for entry in &mut meta {
            entry.id = s.zstr()?;
            if let Some(ref mut name) = entry.name {
                *name = s.zstr()?;
            }
            if let Some(ref mut title) = entry.title {
                *title = s.zstr()?;
            }
        }
        Ok(meta)
    }
}

#[derive(Clone)]
pub struct ComponentMeta<'a> {
    pub len: u32,
    pub kind: crate::ComponentKind,
    pub id: &'a [u8],
    pub name: Option<&'a [u8]>,
    pub title: Option<&'a [u8]>,
}

impl<'a> Debug for ComponentMeta<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("ComponentMeta")
            .field("len", &self.len)
            .field("kind", &self.kind)
            .field("id", &Bstr(self.id))
            .field("name", &self.name.map(Bstr))
            .field("title", &self.title.map(Bstr))
            .finish()
    }
}

pub struct Bundled<'a> {
    offsets: &'a [ComponentOffset],
    end_pos: Pos,
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct ComponentOffset([u8; 4]);

impl ComponentOffset {
    fn cast_slice(arrays: &[[u8; 4]]) -> &[Self] {
        // SAFETY ComponentOffset is repr(transparent)
        unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        }
    }

    fn get(self) -> u32 {
        u32::from_be_bytes(self.0)
    }
}

impl<'a> Bundled<'a> {
    pub fn get(&self, i: u16) -> Option<ComponentP> {
        self.offsets.get(i as usize).map(|off| ComponentP::new(off.get(), self.end_pos))
    }

    pub fn iter(&self) -> BundledIter<'a> {
        BundledIter { offsets: self.offsets.iter(), end_pos: self.end_pos }
    }
}

impl<'a> IntoIterator for Bundled<'a> {
    type Item = ComponentP;
    type IntoIter = BundledIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct BundledIter<'a> {
    offsets: core::slice::Iter<'a, ComponentOffset>,
    end_pos: Pos,
}

impl<'a> Iterator for BundledIter<'a> {
    type Item = ComponentP;

    fn next(&mut self) -> Option<Self::Item> {
        self.offsets.next().map(|off| ComponentP::new(off.get(), self.end_pos))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.offsets.size_hint()
    }
}

impl<'a> core::iter::ExactSizeIterator for BundledIter<'a> {}
impl<'a> core::iter::FusedIterator for BundledIter<'a> {}

impl<'a> core::iter::DoubleEndedIterator for BundledIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.offsets.next_back().map(|off| ComponentP::new(off.get(), self.end_pos))
    }
}

#[derive(Clone, Debug)]
pub struct RawNavm<'a> {
    content: Field<'a>,
}

impl<'a> RawNavm<'a> {
    pub fn bzz(&self) -> &'a [u8] {
        self.content.bytes()
    }

    pub fn decoded<'dec>(&self, decoded: &'dec [u8]) -> Result<DecodedNavm<'dec>, Error> {
        let mut s = self.content.decoded(decoded).split();
        let num_bookmarks = s.u16_be()?;
        let body = s.rest();
        Ok(DecodedNavm { num_bookmarks, body })
    }
}

pub struct DecodedNavm<'a> {
    num_bookmarks: u16,
    body: Field<'a>,
}

impl<'a> DecodedNavm<'a> {
    pub fn num_bookmarks(&self) -> u16 {
        self.num_bookmarks
    }

    pub fn parsing(&self) -> ParsingBookmarks<'a> {
        ParsingBookmarks { remaining: self.num_bookmarks, s: self.body.split() }
    }
}

pub struct ParsingBookmarks<'a> {
    remaining: u16,
    s: SplitInner<'a>,
}

impl<'a> ParsingBookmarks<'a> {
    pub fn parse_next(&mut self) -> Result<Option<Bookmark<'a>>, Error> {
        if self.remaining == 0 {
            return Ok(None);
        }
        self.remaining -= 1;
        let num_children = self.s.byte()?;
        let description_len = self.s.u24_be()?;
        let description = self.s.slice(description_len as usize)?;
        let url_len = self.s.u24_be()?;
        let url = self.s.slice(url_len as usize)?;
        Ok(Some(Bookmark {
            num_children,
            description,
            url,
        }))
    }

    pub fn expected_len(&self) -> u16 {
        self.remaining
    }
}

#[derive(Clone)]
pub struct Bookmark<'a> {
    pub num_children: u8,
    pub description: &'a [u8],
    pub url: &'a [u8],
}

impl<'a> Debug for Bookmark<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Bookmark")
            .field("num_children", &self.num_children)
            .field("description", &Bstr(self.description))
            .field("url", &Bstr(self.url))
            .finish()
    }
}

/// Parsed representation of the start of a component.
pub enum ComponentHead<'a> {
    Djvi {
        elements: ElementP,
    },
    Djvu {
        info: RawInfo<'a>,
        elements: ElementP,
    },
    Thum {
        thumbnails: ThumbnailP,
    },
}

pub struct RawTh44<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawTh44<'a> {
    pub fn parse(&self) -> Result<Iw44<'a>, Error> {
        Iw44::parse_from(self.content.split())
    }

    pub fn after(&self) -> ThumbnailP {
        ThumbnailP::new(self.after_pos, self.end_pos)
    }
}

/// Represents an offset from the beginning of a document or indirect component.
///
/// "Beginning" means the beginning of the *file*---before the magic number.
/// This is how the `DIRM` chunk stores component offsets, and we stick to
/// the same convention.
type Pos = u32;

/// Pointer-like immutable cursor to the start or end of an element.
#[derive(Clone, Debug)]
pub struct ElementP {
    pos: Pos,
    /// The position at which the *containing `FORM`* ends.
    ///
    /// This is one past the last byte of the last element, excluding any trailing padding.
    /// We store this so that `ElementP::is_end` can be implemented.
    end_pos: Pos,
}

impl ElementP {
    fn new(pos: Pos, end_pos: Pos) -> Self {
        Self { pos, end_pos }
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<Element<'a>, ()>, Error> {
        if self.pos == self.end_pos {
            return Ok(Progress::End(()));
        }
        let mut s = split_outer(data, self.pos, Some(self.end_pos));
        let s = &mut s;
        let (kind, content) = try_advance!(s.chunk()?);
        let after_pos = s.pos();
        let end_pos = self.end_pos;
        let element = match &kind {
            b"ANTa" => Element::Anta(RawAnta { content: StringField(content), after_pos, end_pos }),
            b"ANTz" => Element::Antz(RawAntz { content, after_pos, end_pos }),
            b"TXTa" => Element::Txta(RawTxta { content, after_pos, end_pos }),
            b"TXTz" => Element::Txtz(RawTxtz { content, after_pos, end_pos }),
            b"Djbz" => Element::Djbz(RawDjbz { content, after_pos, end_pos }),
            b"Sjbz" => Element::Sjbz(RawSjbz { content, after_pos, end_pos }),
            b"FG44" => Element::Fg44(RawFg44 { content, after_pos, end_pos }),
            b"BG44" => Element::Bg44(RawBg44 { content, after_pos, end_pos }),
            b"FGbz" => Element::Fgbz(RawFgbz { content, after_pos, end_pos }),
            b"INCL" => Element::Incl(RawIncl { content: StringField(content), after_pos, end_pos }),
            b"BGjp" => Element::Bgjp(RawBgjp { content, after_pos, end_pos }),
            b"FGjp" => Element::Fgjp(RawFgjp { content, after_pos, end_pos }),
            b"Smmr" => Element::Smmr(RawSmmr { content, after_pos, end_pos }),
            _ => Element::Unknown(Chunk { kind: Bstr(kind), content, after_pos, end_pos }),
        };
        Ok(advanced(element, s))
    }

    pub fn is_end(&self) -> bool {
        self.pos == self.end_pos
    }
}

/// Pointer-like immutable cursor to the start or end of a component.
pub struct ComponentP {
    pos: Pos,
    end_pos: Pos,
}

impl ComponentP {
    fn new(pos: Pos, end_pos: Pos) -> Self {
        Self { pos, end_pos }
    }

    pub fn offset(&self) -> u32 {
        self.pos
    }

    pub fn is_end(&self) -> bool {
        self.pos == self.end_pos
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<ComponentHead<'a>, ()>, Error> {
        if self.is_end() {
            return Ok(Progress::End(()));
        }
        let mut s = split_outer(data, self.pos, Some(self.end_pos));
        let s = &mut s;
        let (kind, len) = try_advance!(s.form_header()?);
        let head = try_advance!(s.component_head(kind, len)?);
        Ok(advanced(head, s))
    }
}

/// Pointer-like immutable cursor to the start or end of a thumbnail.
pub struct ThumbnailP {
    pos: Pos,
    end_pos: Pos,
}

impl ThumbnailP {
    fn new(pos: Pos, end_pos: Pos) -> Self {
        Self { pos, end_pos }
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<RawTh44<'a>, ()>, Error> {
        if self.is_end() {
            return Ok(Progress::End(()));
        }
        let mut s = split_outer(data, self.pos, Some(self.end_pos));
        let s = &mut s;
        let content = try_advance!(s.specific_chunk(b"TH44")?);
        let thumbnail = RawTh44 {
            content,
            after_pos: s.pos(),
            end_pos: self.end_pos,
        };
        Ok(advanced(thumbnail, s))
    }

    pub fn is_end(&self) -> bool {
        self.pos == self.end_pos
    }
}

fn split_outer(full: &[u8], start_pos: Pos, end_pos: Option<Pos>) -> SplitOuter<'_> {
    SplitOuter {
        full,
        start_pos,
        end_pos,
        by: 0,
    }
}

#[derive(Clone, Debug)]
struct SplitOuter<'a> {
    full: &'a [u8],
    start_pos: Pos,
    end_pos: Option<Pos>,
    by: u32,
}

macro_rules! try_advance_internal {
    ( $x:expr ) => {
        match $x {
            ProgressInternal::None(hint) => return Ok(ProgressInternal::None(hint)),
            ProgressInternal::Advanced(head) => head,
        }
    };
}

impl<'a> SplitOuter<'a> {
    fn pos(&self) -> Pos {
        self.start_pos + self.by
    }

    fn set_distance_to_end(&mut self, len: u32) -> Pos {
        let end_pos = self.pos() + len;
        self.end_pos = Some(end_pos);
        end_pos
    }

    fn remaining(&self) -> &'a [u8] {
        let end = if let Some(pos) = self.end_pos {
            (pos - self.start_pos) as usize
        } else {
            self.full.len()
        };
        &self.full[self.by as usize..end]
    }

    fn header<const N: usize>(&mut self) -> ProgressInternal<&'a [[u8; 4]; N]> {
        let (arrays, _) = crate::shim::slice_as_arrays(self.remaining());
        match crate::shim::slice_split_array(arrays) {
            None => ProgressInternal::None(Some(4 * N)),
            Some((header, _)) => {
                self.by += 4 * N as u32; // XXX
                ProgressInternal::Advanced(header)
            }
        }
    }

    fn field(&mut self, len: u32) -> ProgressInternal<Field<'a>> {
        if self.remaining().len() >= len as usize {
            let field = Field {
                full: self.full,
                start: self.by as usize,
                start_pos: self.pos(),
                end: self.by as usize + len as usize,
                bzz: None,
            };
            self.by += len;
            ProgressInternal::Advanced(field)
        } else {
            ProgressInternal::None(Some(len as usize))
        }
    }

    fn align(&mut self) -> ProgressInternal<()> {
        if self.pos() % 2 != 0 {
            if self.remaining().is_empty() {
                return ProgressInternal::None(None);
            }
            self.by += 1;
        }
        ProgressInternal::Advanced(())
    }

    fn form_header(&mut self) -> Result<ProgressInternal<([u8; 4], u32)>, Error> {
        try_advance_internal!(self.align());
        let &[id, xs, kind] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if &id != b"FORM" {
            return Err(error_placeholder());
        }
        if !is_potential_chunk_id(kind) {
            return Err(error_placeholder());
        }
        Ok(ProgressInternal::Advanced((kind, len - 4)))
    }

    fn magic_form_header(&mut self) -> Result<ProgressInternal<([u8; 4], u32)>, Error> {
        // no need to align here, magic number always occurs at the very beginning
        let &[magic, id, xs, kind] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if &magic != b"AT&T" {
            return Err(error_placeholder());
        }
        if &id != b"FORM" {
            return Err(error_placeholder());
        }
        if !is_potential_chunk_id(kind) {
            return Err(error_placeholder());
        }
        Ok(ProgressInternal::Advanced((kind, len - 4)))
    }

    fn chunk(&mut self) -> Result<ProgressInternal<([u8; 4], Field<'a>)>, Error> {
        try_advance_internal!(self.align());
        let &[id, xs] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if !is_potential_chunk_id(id) {
            return Err(error_placeholder());
        }
        let content = try_advance_internal!(self.field(len));
        Ok(ProgressInternal::Advanced((id, content)))
    }

    fn specific_chunk(&mut self, expected: &[u8; 4]) -> Result<ProgressInternal<Field<'a>>, Error> {
        let (id, content) = try_advance_internal!(self.chunk()?);
        if &id != expected {
            return Err(error_placeholder());
        }
        Ok(ProgressInternal::Advanced(content))
    }

    fn peek_chunk(&self) -> Result<ProgressInternal<[u8; 4]>, Error> {
        let mut s = self.clone();
        try_advance_internal!(s.align());
        let &[id] = try_advance_internal!(s.header());
        if !is_potential_chunk_id(id) {
            return Err(error_placeholder());
        }
        Ok(ProgressInternal::Advanced(id))
    }

    fn component_head(&mut self, kind: [u8; 4], len: u32) -> Result<ProgressInternal<ComponentHead<'a>>, Error> {
        let end_pos = self.set_distance_to_end(len);
        let head = match &kind {
            b"DJVI" => { 
                let elements = ElementP::new(self.pos(), end_pos);
                ComponentHead::Djvi { elements }
            }
            b"DJVU" => {
                let content = try_advance_internal!(self.specific_chunk(b"INFO")?);
                let info = RawInfo { content };
                let elements = ElementP::new(self.pos(), end_pos);
                ComponentHead::Djvu { info, elements }
            }
            b"THUM" => {
                let thumbnails = ThumbnailP::new(self.pos(), end_pos);
                ComponentHead::Thum { thumbnails }
            }
            _ => return Err(error_placeholder()),
        };
        Ok(ProgressInternal::Advanced(head))
    }
}

#[derive(Clone, Copy, Debug)]
struct Field<'a> {
    full: &'a [u8],
    start: usize,
    start_pos: Pos,
    end: usize,
    bzz: Option<(Pos, Pos)>,
}

impl<'a> Field<'a> {
    fn bytes(&self) -> &'a [u8] {
        &self.full[self.start..self.end]
    }

    fn split(self) -> SplitInner<'a> {
        SplitInner {
            parent: self,
            by: 0,
        }
    }

    fn decoded<'dec>(self, decoded: &'dec [u8]) -> Field<'dec> {
        Field {
            full: decoded,
            start: 0,
            start_pos: 0,
            end: decoded.len(),
            bzz: Some((self.start_pos, self.start_pos + (self.end - self.start) as u32)), // XXX
        }
    }
}

// TODO custom Debug impl using Bstr
#[derive(Clone, Copy)]
struct StringField<'a>(Field<'a>);

struct SplitInner<'a> {
    parent: Field<'a>,
    by: u32,
}

impl<'a> SplitInner<'a> {
    fn remaining(&self) -> &'a [u8] {
        &self.parent.full[self.parent.start + self.by as usize..self.parent.end]
    }

    fn array<const N: usize>(&mut self) -> Result<&'a [u8; N], Error> {
        if let Some((array, _)) = crate::shim::slice_split_array(self.remaining()) {
            self.by += N as u32; // XXX
            Ok(array)
        } else {
            Err(error_placeholder())
        }
    }

    fn slice_of_arrays<const N: usize>(&mut self, n: usize) -> Result<&'a [[u8; N]], Error> {
        let (arrays, _) = crate::shim::slice_as_arrays(self.remaining());
        if let Some(arrays) = arrays.get(..n) {
            self.by += n as u32 * N as u32; // XXX
            Ok(arrays)
        } else {
            Err(error_placeholder())
        }
    }

    fn slice(&mut self, n: usize) -> Result<&'a [u8], Error> {
        if let Some(slice) = self.remaining().get(..n) {
            self.by += n as u32; // XXX
            Ok(slice)
        } else {
            Err(error_placeholder())
        }
    }

    fn zstr(&mut self) -> Result<&'a [u8], Error> {
        if let Some(i) = self.remaining().iter().position(|&x| x == b'\0') {
            let s = &self.remaining()[..i]; // XXX
            self.by += i as u32 + 1;
            Ok(s)
        } else {
            Err(error_placeholder())
        }
    }

    fn byte(&mut self) -> Result<u8, Error> {
        let &[x] = self.array()?;
        Ok(x)
    }

    fn u16_be(&mut self) -> Result<u16, Error> {
        let &xs = self.array()?;
        Ok(u16::from_be_bytes(xs))
    }

    fn u16_le(&mut self) -> Result<u16, Error> {
        let &xs = self.array()?;
        Ok(u16::from_le_bytes(xs))
    }

    fn u24_be(&mut self) -> Result<u32, Error> {
        let &[x, y, z] = self.array()?;
        Ok(u32::from_be_bytes([0, x, y, z]))
    }

    fn rest(self) -> Field<'a> {
        Field {
            full: self.parent.full,
            start: self.parent.start + self.by as usize,
            start_pos: self.parent.start_pos + self.by,
            end: self.parent.end,
            bzz: self.parent.bzz,
        }
    }

    fn rest_arrays<const N: usize>(self) -> Result<&'a [[u8; N]], Error> {
        let (arrays, rest) = crate::shim::slice_as_arrays(self.remaining());
        if rest.is_empty() {
            Ok(arrays)
        } else {
            Err(error_placeholder())
        }
    }
}

mod ant;
pub use ant::ParsingAnnots;
