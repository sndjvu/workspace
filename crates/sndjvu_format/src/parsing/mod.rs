//! Low-level parser for the DjVu transfer format.
//!
//! "Low-level" means that only the grittiest details of the file format are abstracted away,
//! and that flexibility is prioritized over convenience in the design of the interface.
//! Callers only need to load a relatively small amount of data into memory to begin parsing, and
//! can choose exactly which parts of the document they want to parse.
//!
//! Entry points are the [`document`] and [`indirect_component`] free functions; other key items
//! are [`Progress`] and the pointer-like types [`ComponentP`], [`ElementP`].
//!
//! ## Incremental parsing
//!
//! A DjVu document in the transfer format is a sequence of bytes. A major design goal of this
//! module is that you can parse parts of a document without having all the bytes in memory at any
//! one time. The parser keeps track of a correspondence between positions in an abstract byte
//! stream and features of the document structure, and the caller provides a chunk of bytes from
//! a specific position when it wants to parse the corresponding feature. The functions that work
//! this way are:
//!
//! - [`document`]
//! - [`indirect_component`]
//! - [`ComponentP::feed`]
//! - [`ElementP::feed`]
//!
//! The return type of each of these looks like `Result<Progress<T, N>, Error>`, which is how we
//! represent this set of possible outcomes:
//!
//! - some part of the provided data was invalid (`Result::Err`)
//! - not enough bytes were provided to parse the requested document feature
//!   (`Result::Ok(Progress::None)`)
//! - parsing succeeded (`Result::Ok(Progress::Advanced)`)
//! - there is no feature to parse at this position (`Result::Ok(Progress::End)`)---this is a
//!   possible outcome only for [`ComponentP::feed`] and [`ElementP::feed`]

use crate::{
    Bstr, PaletteIndex, PageRotation, InfoVersion,
    TxtVersion, Zone, Iw44ColorSpace, Iw44Version, FgbzVersion,
    PaletteEntry, DirmVersion, ComponentKind, Never,
};
use core::fmt::{Debug, Display, Formatter};
use core::num::NonZeroU8;
use alloc::vec::Vec;
#[cfg(feature = "backtrace")]
use std::backtrace::Backtrace;

/// The outcome of a parsing operation, if no [`Error`] was encountered.
#[derive(Debug)]
pub enum Progress<T, N = Never> {
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
    /// If `N` is uninhabited (see [`Never`]), this variant cannot be constructed.
    End(N),
}

fn advanced<T, N>(head: T, s: &SplitOuter<'_>) -> Progress<T, N> {
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

/// An error encountered while parsing.
///
/// Contains a backtrace if the `backtrace` crate feature is enabled.
pub struct Error {
    #[cfg(feature = "backtrace")]
    backtrace: Backtrace,
}

// Backtrace before Rust 1.73.0 wasn't RefUnwindSafe. Explicitly implement it for Error
// to avoid either increasing MSRV or having the properties of this type depend on the Rust
// version.
impl core::panic::RefUnwindSafe for Error {}

impl Error {
    fn placeholder() -> Self {
        Self {
            #[cfg(feature = "backtrace")]
            backtrace: Backtrace::capture(),
        }
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "parsing failed")?;
        #[cfg(feature = "backtrace")] {
            write!(f, "\n\n{}", self.backtrace)?;
        }
        Ok(())
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "parsing failed")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

/// Start parsing a DjVu document from some bytes.
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
            let components = ComponentP::new(s.pos(), end_pos);
            DocumentHead::MultiPage { dirm, navm, components }
        }
        _ => return Err(Error::placeholder()),
    };
    Ok(advanced(head, s))
}

/// Start parsing a component of an indirect multi-page DjVu document from some bytes.
pub fn indirect_component(data: &[u8]) -> Result<Progress<ComponentHead<'_>>, Error> {
    let mut s = split_outer(data, 0, None);
    let s = &mut s;
    let (kind, len) = try_advance!(s.magic_form_header()?);
    let head = try_advance!(s.component_head(kind, len)?);
    Ok(advanced(head, s))
}


/// Parsed representation of the start of a document.
#[derive(Debug)]
pub enum DocumentHead<'a> {
    SinglePage {
        info: RawInfo<'a>,
        elements: ElementP,
    },
    MultiPage {
        dirm: RawDirm<'a>,
        navm: Option<RawNavm<'a>>,
        components: ComponentP,
    },
}

/// Unparsed representation of an `INFO` chunk.
#[derive(Clone, Debug)]
pub struct RawInfo<'a> {
    content: Field<'a>,
}

impl<'a> RawInfo<'a> {
    /// Attempt to parse the `INFO` chunk.
    ///
    /// Any extra data in the chunk is returned in the second field of the tuple.
    pub fn parse(&self) -> Result<(Info, &'a [u8]), Error> {
        let mut s = self.content.split();
        let width = s.u16_be()?;
        let height = s.u16_be()?;
        let &[minor, major] = s.array()?;
        let dpi = s.u16_le()?;
        let gamma = s.byte()?;
        let flags = s.byte()?;
        let rotation = match flags & 0b111 {
            1 => PageRotation::Up,
            6 => PageRotation::Ccw,
            2 => PageRotation::Down,
            5 => PageRotation::Cw,
            _ => PageRotation::Up, // see djvuchanges.txt
        };
        let extra = s.remaining();
        Ok((Info {
            width,
            height,
            version: InfoVersion { major, minor },
            dpi,
            gamma,
            rotation,
        }, extra))
    }
}

/// Parsed representation of an `INFO` chunk.
#[derive(Debug)]
pub struct Info {
    pub width: u16,
    pub height: u16,
    pub version: InfoVersion,
    pub dpi: u16,
    pub gamma: u8,
    pub rotation: PageRotation,
}

fn is_potential_chunk_id(xs: [u8; 4]) -> bool {
    xs.iter().all(u8::is_ascii_alphanumeric)
}

/// An unparsed chunk that represents a page element.
#[derive(Debug)]
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
    /// A chunk whose type was not recognized.
    ///
    /// Note that we signal an error instead of returning this variant if the chunk type field
    /// contains non-ASCII-alphanumeric bytes.
    Unknown(Chunk<'a>),
}

impl<'a> Element<'a> {
    /// Get a pointer to the element after this one, if it exists.
    ///
    /// If this is the last element, the returned pointer will be "one-past-the-end" and calling
    /// [`ElementP::feed`] will return `Ok(Progress::End(()))`.
    pub fn after(&self) -> ElementP {
        match *self {
            | Self::Anta(RawAnta { after_pos, end_pos, .. })
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
            | Self::Unknown(Chunk { after_pos, end_pos, .. }) => ElementP::new(after_pos, end_pos),
        }
    }
}

/// Unparsed representation of an `ANTa` chunk.
#[derive(Debug)]
pub struct RawAnta<'a> {
    content: StringField<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawAnta<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.0.bytes()
    }
}

/// Unparsed representation of an `ANTz` chunk.
#[derive(Debug)]
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

/// Contents of an `ANTz` chunk after BZZ decompression.
#[derive(Debug)]
pub struct DecodedAntz<'a> {
    content: StringField<'a>,
}

impl<'a> DecodedAntz<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.0.bytes()
    }
}

/// Unparsed representation of an `TXTa` chunk.
#[derive(Debug)]
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

/// Unparsed representation of a `TXTz` chunk.
#[derive(Debug)]
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

/// Parsed representation of a `TXTa` or (decompressed) `TXTz` chunk.
pub struct Txt<'a> {
    pub text: &'a [u8],
    pub version: TxtVersion,
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
        let version = s.byte().map(TxtVersion)?;
        let raw_zones = s.rest_arrays()?;
        let zones = Zone::cast_slice(raw_zones).ok_or_else(Error::placeholder)?;
        Ok(Txt {
            text,
            version,
            zones,
        })
    }
}

/// Represents a `Djbz` chunk, which doesn't need further parsing.
#[derive(Debug)]
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

/// Represents an `Sjbz` chunk, which doesn't need further parsing.
#[derive(Debug)]
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

/// Unparsed representation of an `FG44` chunk.
#[derive(Debug)]
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

/// Unparsed representation of a `BG44` chunk.
#[derive(Debug)]
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

/// Parsed representation of an `FG44`, `BG44`, or `TH44` chunk.
#[derive(Debug)]
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
                Iw44ColorSpace::Gray
            } else {
                Iw44ColorSpace::YCbCr
            };
            let major = byte & 0b0111_1111;
            let minor = s.byte()?;
            let version = Iw44Version { major, minor };
            let width = s.u16_be()?;
            let height = s.u16_be()?;
            let initial_cdc = s.byte()? & 0x7f;
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

/// Subtypes of the `FG44`/`BG44`/`TH44` chunk types.
#[derive(Clone, Copy, Debug)]
pub enum Iw44Kind {
    Head {
        version: Iw44Version,
        colors: Iw44ColorSpace,
        width: u16,
        height: u16,
        initial_cdc: u8,
    },
    Tail { serial: NonZeroU8 },
}

/// Unparsed representation of an `FGbz` chunk.
#[derive(Debug)]
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
        let version = FgbzVersion(byte & 0b0111_1111);
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

/// Parsed representation of an `FGbz` chunk.
#[derive(Debug)]
pub struct Fgbz<'a> {
    pub version: FgbzVersion,
    pub palette: &'a [PaletteEntry],
    pub indices: Option<FgbzIndices<'a>>,
}

/// Compressed portion of an `FGbz` chunk.
#[derive(Debug)]
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

/// Represents an `INCL` chunk, which doesn't need parsing.
#[derive(Debug)]
pub struct RawIncl<'a> {
    content: StringField<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawIncl<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.content.0.bytes()
    }
}

/// Represents a `BGjp` chunk, which doesn't need parsing.
#[derive(Debug)]
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

/// Represents an `FGjp` chunk, which doesn't need parsing.
#[derive(Debug)]
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

/// Unparsed representation of an `Smmr` chunk.
#[derive(Debug)]
pub struct RawSmmr<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> RawSmmr<'a> {
    pub fn parse(&self) -> Result<Smmr<'a>, Error> {
        let mut s = self.content.split();
        let magic = s.array()?;
        if magic != b"MMR" {
            return Err(Error::placeholder());
        }
        let flags = s.byte()?;
        let is_reverse_video = flags & 0b1 != 0;
        let is_striped = flags & 0b10 != 0;
        let width = s.u16_be()?;
        let height = s.u16_be()?;
        let rest = s.rest();
        let body = if is_striped {
            let mut s = rest.split();
            let rows_per_stripe = s.u16_be()?;
            let rest = s.rest();
            SmmrBody::Striped(MmrStripes { rows_per_stripe, rest })
        } else {
            SmmrBody::Bulk(Mmr { data: rest })
        };
        Ok(Smmr {
            is_reverse_video,
            width,
            height,
            body,
        })
    }
}

/// Parsed representation of an `Smmr` chunk.
#[derive(Debug)]
pub struct Smmr<'a> {
    pub is_reverse_video: bool,
    pub width: u16,
    pub height: u16,
    pub body: SmmrBody<'a>,
}

/// Possible formats for the MMR data in an `Smmr` chunk.
#[derive(Debug)]
pub enum SmmrBody<'a> {
    Bulk(Mmr<'a>),
    Striped(MmrStripes<'a>),
}

/// Raw MMR-compressed data from an `Smmr` chunk.
#[derive(Debug)]
pub struct Mmr<'a> {
    data: Field<'a>,
}

impl<'a> Mmr<'a> {
    pub fn bytes(&self) -> &'a [u8] {
        self.data.bytes()
    }
}

/// "Striped" MMR-compressed data from an `Smmr` chunk.
#[derive(Debug)]
pub struct MmrStripes<'a> {
    rows_per_stripe: u16,
    rest: Field<'a>,
}

impl<'a> MmrStripes<'a> {
    pub fn rows_per_stripe(&self) -> u16 {
        self.rows_per_stripe
    }

    pub fn parsing(&self) -> ParsingStripes<'a> {
        ParsingStripes { s: self.rest.split() }
    }
}

/// Fallible iterator for parsing the stripe data from an `Smmr` chunk.
#[derive(Debug)]
pub struct ParsingStripes<'a> {
    s: SplitInner<'a>,
}

impl<'a> ParsingStripes<'a> {
    pub fn parse_next(&mut self) -> Result<Option<Mmr<'a>>, Error> {
        if self.s.remaining().is_empty() {
            return Ok(None);
        }
        let len = self.s.u32_be()?;
        let data = self.s.field(len as usize)?;
        Ok(Some(Mmr { data }))
    }
}

/// A chunk from a `DJVU` or `DJVI` component whose type wasn't recognized.
#[derive(Debug)]
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

/// Unparsed representation of the `DIRM` chunk.
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
        let version = DirmVersion(flags & 0b0111_1111);
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

/// Parsed representation of the `DIRM` chunk.
#[derive(Debug)]
pub struct Dirm<'a> {
    pub version: DirmVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'a>>,
    pub extra: DirmExtra<'a>,
}

/// Compressed portion of the `DIRM` chunk.
#[derive(Debug)]
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
            kind: ComponentKind::Djvi,
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
                0 => ComponentKind::Djvi,
                1 => ComponentKind::Djvu,
                2 => ComponentKind::Thum,
                _ => return Err(Error::placeholder()),
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

/// Basic metadata about one component of a multi-page document.
#[derive(Clone)]
pub struct ComponentMeta<'a> {
    pub len: u32,
    pub kind: ComponentKind,
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

/// Data from the `DIRM` chunk that's only present in bundled documents.
#[derive(Debug)]
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

/// Iterator over "pointers" to each component of a bundled multi-page document.
#[derive(Debug)]
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

/// Unparsed representation of the `NAVM` chunk.
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

/// Contents of the `NAVM` chunk after BZZ decompression.
#[derive(Debug)]
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

/// Fallible iterator for parsing the bookmarks from a decompressed `NAVM` chunk.
#[derive(Debug)]
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

/// A single bookmark record from the (decompressed) `NAVM` chunk.
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
#[derive(Debug)]
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

/// Unparsed representation of an `Smmr` chunk.
#[derive(Debug)]
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

    pub fn offset(&self) -> u32 {
        self.pos
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
#[derive(Debug)]
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

    pub fn is_end(&self) -> Option<bool> {
        if self.pos > self.end_pos {
            return None;
        }
        Some(self.pos == self.end_pos)
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<ComponentHead<'a>, ()>, Error> {
        let Some(is_end) = self.is_end() else {
            return Err(Error::placeholder());
        };
        if is_end {
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
#[derive(Debug)]
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
        let end_pos = self.pos().saturating_add(len);
        self.end_pos = Some(end_pos);
        end_pos
    }

    fn remaining(&self) -> &'a [u8] {
        let mut end = self.full.len();
        if let Some(pos) = self.end_pos {
            end = end.min((pos - self.start_pos) as usize);
        }
        &self.full[self.by as usize..end]
    }

    fn header<const N: usize>(&mut self) -> ProgressInternal<&'a [[u8; 4]; N]> {
        let (arrays, _) = crate::shim::slice_as_arrays(self.remaining());
        match crate::shim::slice_split_array(arrays) {
            None => ProgressInternal::None(None),
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
            ProgressInternal::None(None)
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
            return Err(Error::placeholder());
        }
        if !is_potential_chunk_id(kind) {
            return Err(Error::placeholder());
        }
        if len < 4 {
            return Err(Error::placeholder());
        }
        Ok(ProgressInternal::Advanced((kind, len - 4)))
    }

    fn magic_form_header(&mut self) -> Result<ProgressInternal<([u8; 4], u32)>, Error> {
        // no need to align here, magic number always occurs at the very beginning
        let &[magic, id, xs, kind] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if &magic != b"AT&T" {
            return Err(Error::placeholder());
        }
        if &id != b"FORM" {
            return Err(Error::placeholder());
        }
        if !is_potential_chunk_id(kind) {
            return Err(Error::placeholder());
        }
        if len < 4 {
            return Err(Error::placeholder());
        }
        Ok(ProgressInternal::Advanced((kind, len - 4)))
    }

    fn chunk(&mut self) -> Result<ProgressInternal<([u8; 4], Field<'a>)>, Error> {
        try_advance_internal!(self.align());
        let &[id, xs] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if !is_potential_chunk_id(id) {
            return Err(Error::placeholder());
        }
        let content = try_advance_internal!(self.field(len));
        Ok(ProgressInternal::Advanced((id, content)))
    }

    fn specific_chunk(&mut self, expected: &[u8; 4]) -> Result<ProgressInternal<Field<'a>>, Error> {
        let (id, content) = try_advance_internal!(self.chunk()?);
        if &id != expected {
            return Err(Error::placeholder());
        }
        Ok(ProgressInternal::Advanced(content))
    }

    fn peek_chunk(&self) -> Result<ProgressInternal<[u8; 4]>, Error> {
        let mut s = self.clone();
        try_advance_internal!(s.align());
        let &[id] = try_advance_internal!(s.header());
        if !is_potential_chunk_id(id) {
            return Err(Error::placeholder());
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
            _ => return Err(Error::placeholder()),
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

    fn decoded(self, decoded: &[u8]) -> Field<'_> {
        Field {
            full: decoded,
            start: 0,
            start_pos: 0,
            end: decoded.len(),
            bzz: Some((self.start_pos, self.start_pos + (self.end - self.start) as u32)), // XXX
        }
    }
}

#[derive(Clone, Copy)]
struct StringField<'a>(Field<'a>);

impl<'a> Debug for StringField<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("StringField")
            .field("full", &Bstr(self.0.full))
            .field("start", &self.0.start)
            .field("start_pos", &self.0.start_pos)
            .field("end", &self.0.end)
            .field("bzz", &self.0.bzz)
            .finish()
    }
}

#[derive(Debug)]
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
            Err(Error::placeholder())
        }
    }

    fn slice_of_arrays<const N: usize>(&mut self, n: usize) -> Result<&'a [[u8; N]], Error> {
        let (arrays, _) = crate::shim::slice_as_arrays(self.remaining());
        if let Some(arrays) = arrays.get(..n) {
            self.by += n as u32 * N as u32; // XXX
            Ok(arrays)
        } else {
            Err(Error::placeholder())
        }
    }

    fn field(&mut self, n: usize) -> Result<Field<'a>, Error> {
        if self.remaining().get(..n).is_some() {
            let field = Field {
                full: self.parent.full,
                start: self.parent.start + self.by as usize,
                start_pos: self.parent.start_pos + self.by,
                end: self.parent.end + self.by as usize + n,
                bzz: self.parent.bzz,
            };
            self.by += n as u32; // XXX
            Ok(field)
        } else {
            Err(Error::placeholder())
        }
    }

    fn slice(&mut self, n: usize) -> Result<&'a [u8], Error> {
        if let Some(slice) = self.remaining().get(..n) {
            self.by += n as u32; // XXX
            Ok(slice)
        } else {
            Err(Error::placeholder())
        }
    }

    fn zstr(&mut self) -> Result<&'a [u8], Error> {
        if let Some(i) = self.remaining().iter().position(|&x| x == b'\0') {
            let s = &self.remaining()[..i]; // XXX
            self.by += i as u32 + 1;
            Ok(s)
        } else {
            Err(Error::placeholder())
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

    fn u32_be(&mut self) -> Result<u32, Error> {
        let &xs = self.array()?;
        Ok(u32::from_be_bytes(xs))
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
            Err(Error::placeholder())
        }
    }
}

/// Start parsing annotations from the string content of a `ANTa` or `ANTz` chunk.
pub fn annots(s: &str) -> ParsingAnnots<'_> {
    ParsingAnnots::new(s, crate::annot::Escaping::Djvulibre)
}

pub fn annots_lizardtech(s: &str) -> ParsingAnnots<'_> {
    ParsingAnnots::new(s, crate::annot::Escaping::Lizardtech)
}

mod ant;
pub use ant::ParsingAnnots;
