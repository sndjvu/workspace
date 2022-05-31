//! Low-level parser for the DjVu transfer format.
//!
//! Entry points to this module are [`document`] and [`indirect_component`].
//!
//! ## Parsing strategy
//!
//! The goal is to provide everything needed parse a DjVu document
//! while allowing the caller maximum flexibility in memory management
//! and I/O handling.
//!
//! - Callers don't need to load an entire document or even an entire
//!   component into memory at once. Generally, only a single chunk
//!   needs to be loaded to make progress. This is important for
//!   situations where a document is being received piecemeal over
//!   the wire, and you want to start parsing it as soon as possible,
//!   and also desirable whenever you're trying to control memory usage.
//! - Parsing functions accept `&[u8]`, the lowest common denominator
//!   for in-memory binary data.
//! - There are no references to `std::fs`, `std::net`, `std::io`, etc.
//!   In fact, the whole library is `no_std` by default.
//! - BZZ decoding is up to the caller. Once decoded, you can pass the
//!   raw data to the appropriate function as a `&[u8]`. The sndjvu_codec
//!   library provides everything you need, or you can plug in a different
//!   decoder.
//!
//! ## Pointers
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
//! Of note, all these types can validly be "one-past-the-end". This extends the analogy with
//! pointers and is necessary anyway to support documents with no components and components with no
//! elements or thumbnails. Trying to `feed` a one-past-the-end value will always return
//! [`Progress::End`].

// FIXME the DjVu spec is unclear about whether a trailing padding byte at the end of a FORM
// chunk's content is allowed/required. we should accomodate the presence of this kind of
// padding

// FIXME figure out a better general strategy for working with the `Progress` type and
// computing `by` and `hint`. note that right now we're computing `Advanced { by }` even
// for internal functions where that value will never be examined

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

pub struct Error {
}

pub fn document(data: &[u8]) -> Result<Progress<DocumentHead<'_>>, Error> {
    let mut s = split_outer(data, 0, None); // don't know end_pos yet
    let s = &mut s;
    let (kind, len) = try_advance!(s.magic_form_header()?);
    let end_pos = s.set_distance_to_end(len);
    let head = match &kind {
        b"DJVU" => {
            let content = try_advance!(s.specific_chunk(b"INFO")?);
            let info = InfoChunk { content };
            let elements = ElementP::new(s.pos(), end_pos);
            DocumentHead::SinglePage { info, elements }
        }
        b"DJVM" => {
            let content = try_advance!(s.specific_chunk(b"DIRM")?);
            let dirm = DirmChunk { content, end_pos };
            let kind = try_advance!(s.peek_chunk()?);
            let navm = if &kind == b"NAVM" {
                let content = try_advance!(s.specific_chunk(b"NAVM")?);
                Some(Navm { content })
            } else {
                None
            };
            DocumentHead::MultiPage { dirm, navm }

        }
        _ => return Err(Error {}),
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
pub enum DocumentHead<'a> {
    SinglePage {
        info: InfoChunk<'a>,
        elements: ElementP,
    },
    MultiPage {
        dirm: DirmChunk<'a>,
        navm: Option<Navm<'a>>,
    },
}

pub struct InfoChunk<'a> {
    content: Field<'a>,
}

pub struct Info<'a> {
    content: Field<'a>,
    pub width: u16,
    pub height: u16,
    pub version: crate::InfoVersion,
    pub dpi: u16,
    pub gamma: u8,
    pub rotation: crate::PageRotation,
}

impl<'a> InfoChunk<'a> {
    pub fn parse(&self) -> Result<Info<'a>, Error> {
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
            content: self.content,
            width,
            height,
            version: crate::InfoVersion { major, minor },
            dpi,
            gamma,
            rotation,
        })
    }
}

fn is_potential_chunk_id(xs: [u8; 4]) -> bool {
    xs.iter().all(u8::is_ascii_alphanumeric)
}

/// Parsed representation of an element of a page.
pub enum Element<'a> {
    Anta(AntaChunk<'a>),
    Antz(AntzChunk<'a>),
    Txta(TxtaChunk<'a>),
    Txtz(TxtzChunk<'a>),
    Djbz(Djbz<'a>),
    Sjbz(Sjbz<'a>),
    Fg44(Fg44Chunk<'a>),
    Bg44(Bg44Chunk<'a>),
    Fgbz(FgbzChunk<'a>),
    Incl(Incl<'a>),
    Bgjp(Bgjp<'a>),
    Fgjp(Fgjp<'a>),
    Smmr(SmmrChunk<'a>),
    Unknown(Chunk<'a>),
}

impl<'a> Element<'a> {
    pub fn after(&self) -> ElementP {
        match *self {
            Self::Anta(AntaChunk { after_pos, end_pos, .. })
            | Self::Antz(AntzChunk { after_pos, end_pos, .. })
            | Self::Txta(TxtaChunk { after_pos, end_pos, .. })
            | Self::Txtz(TxtzChunk { after_pos, end_pos, .. })
            | Self::Djbz(Djbz { after_pos, end_pos, .. })
            | Self::Sjbz(Sjbz { after_pos, end_pos, .. })
            | Self::Fg44(Fg44Chunk { after_pos, end_pos, .. })
            | Self::Bg44(Bg44Chunk { after_pos, end_pos, .. })
            | Self::Fgbz(FgbzChunk { after_pos, end_pos, .. })
            | Self::Incl(Incl { after_pos, end_pos, .. })
            | Self::Bgjp(Bgjp { after_pos, end_pos, .. })
            | Self::Fgjp(Fgjp { after_pos, end_pos, .. })
            | Self::Smmr(SmmrChunk { after_pos, end_pos, .. })
            | Self::Unknown(Chunk { after_pos, end_pos, .. })
                => ElementP::new(after_pos, end_pos),
        }
    }
}

pub struct AntaChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct AntzChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct TxtaChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct TxtzChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Djbz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Sjbz<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Fg44Chunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Bg44Chunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct FgbzChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> FgbzChunk<'a> {
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
        todo!()
    }

    pub fn parse_decoded<'dec>(&self, decoded: &'dec [u8]) -> Result<&'dec [PaletteIndex], Error> {
        let mut s = self.content.split_decoded(decoded);
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

pub struct Incl<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Bgjp<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Fgjp<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct SmmrChunk<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Chunk<'a> {
    kind: [u8; 4],
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct DirmChunk<'a> {
    content: Field<'a>,
    end_pos: Pos,
}

impl<'a> DirmChunk<'a> {
    pub fn parse(&self) -> Result<Dirm<'a>, Error> {
        let mut s = self.content.split();
        let flags = s.byte()?;
        let is_bundled = flags >> 7 != 0;
        let version = crate::DirmVersion(flags & 0b0111_1111);
        let num_components = s.u16_be()?;
        let bundled = if is_bundled {
            let arrays = s.slice_of_arrays(num_components as usize)?;
            let offsets = ComponentOffset::cast_slice(arrays);
            Some(Bundled { offsets, end_pos: self.end_pos })
        } else {
            None
        };
        let bzz = s.rest();
        Ok(Dirm {
            content: self.content,
            version,
            num_components,
            bundled,
            bzz,
        })
    }
}

pub struct Dirm<'a> {
    content: Field<'a>,
    pub version: crate::DirmVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'a>>,
    bzz: Field<'a>,
}

impl<'a> Dirm<'a> {
    pub fn bzz_extra(&self) -> &'a [u8] {
        todo!()
    }

    pub fn parse_decoded_extra<'dec>(&self, decoded: &'dec [u8]) -> Result<alloc::vec::Vec<ComponentMeta<'dec>>, Error> {
        todo!()
    }
}

pub struct ComponentMeta<'a> {
    pub len: u32,
    pub kind: crate::ComponentKind,
    pub id: &'a [u8],
    pub name: Option<&'a [u8]>,
    pub title: Option<&'a [u8]>,
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

pub struct Navm<'a> {
    content: Field<'a>,
}

/// Parsed representation of the start of a component.
pub enum ComponentHead<'a> {
    Djvi {
        elements: ElementP,
    },
    Djvu {
        info: InfoChunk<'a>,
        elements: ElementP,
    },
    Thum {
        thumbnails: ThumbnailP,
    },
}

/// Parsed representation of a thumbnail image (`TH44` chunk).
pub struct Thumbnail<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

impl<'a> Thumbnail<'a> {
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
        let element = match &kind {
            b"INCL" => {
                let incl = Incl {
                    content,
                    after_pos: s.pos(),
                    end_pos: self.end_pos
                };
                Element::Incl(incl)
            }
            _ => return Err(Error {}),
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

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<Thumbnail<'a>, ()>, Error> {
        if self.is_end() {
            return Ok(Progress::End(()));
        }
        let mut s = split_outer(data, self.pos, Some(self.end_pos));
        let s = &mut s;
        let content = try_advance!(s.specific_chunk(b"TH44")?);
        let thumbnail = Thumbnail {
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
        let (arrays, _) = crate::shim::as_arrays(self.remaining());
        match crate::shim::split_array(arrays) {
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
            return Err(Error {});
        }
        if !is_potential_chunk_id(kind) {
            return Err(Error {});
        }
        Ok(ProgressInternal::Advanced((kind, len - 4)))
    }

    fn magic_form_header(&mut self) -> Result<ProgressInternal<([u8; 4], u32)>, Error> {
        // no need to align here, magic number always occurs at the very beginning
        let &[magic, id, xs, kind] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if &magic != b"AT&T" {
            return Err(Error {});
        }
        if &id != b"FORM" {
            return Err(Error {});
        }
        if !is_potential_chunk_id(kind) {
            return Err(Error {});
        }
        Ok(ProgressInternal::Advanced((kind, len)))
    }

    fn chunk(&mut self) -> Result<ProgressInternal<([u8; 4], Field<'a>)>, Error> {
        try_advance_internal!(self.align());
        let &[id, xs] = try_advance_internal!(self.header());
        let len = u32::from_be_bytes(xs);
        if !is_potential_chunk_id(id) {
            return Err(Error {});
        }
        let content = try_advance_internal!(self.field(len));
        Ok(ProgressInternal::Advanced((id, content)))
    }

    fn specific_chunk(&mut self, expected: &[u8; 4]) -> Result<ProgressInternal<Field<'a>>, Error> {
        let (id, content) = try_advance_internal!(self.chunk()?);
        if &id != expected {
            return Err(Error {});
        }
        Ok(ProgressInternal::Advanced(content))
    }

    fn peek_chunk(&self) -> Result<ProgressInternal<[u8; 4]>, Error> {
        let mut s = self.clone();
        try_advance_internal!(s.align());
        let &[id] = try_advance_internal!(s.header());
        if !is_potential_chunk_id(id) {
            return Err(Error {});
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
                let info = InfoChunk { content };
                let elements = ElementP::new(self.pos(), end_pos);
                ComponentHead::Djvu { info, elements }
            }
            b"THUM" => {
                let thumbnails = ThumbnailP::new(self.pos(), end_pos);
                ComponentHead::Thum { thumbnails }
            }
            _ => return Err(Error {}),
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
}

impl<'a> Field<'a> {
    fn split(self) -> SplitInner<'a> {
        SplitInner {
            parent: self,
            by: 0,
        }
    }

    fn split_decoded<'dec>(self, decoded: &'dec [u8]) -> SplitInner<'dec> {
        todo!()
    }
}

struct SplitInner<'a> {
    parent: Field<'a>,
    by: u32,
}

impl<'a> SplitInner<'a> {
    fn remaining(&self) -> &'a [u8] {
        &self.parent.full[self.parent.start + self.by as usize..self.parent.end]
    }

    fn array<const N: usize>(&mut self) -> Result<&'a [u8; N], Error> {
        if let Some((array, _)) = crate::shim::split_array(self.remaining()) {
            self.by += N as u32; // XXX
            Ok(array)
        } else {
            Err(Error {})
        }
    }

    fn slice_of_arrays<const N: usize>(&mut self, n: usize) -> Result<&'a [[u8; N]], Error> {
        let (arrays, _) = crate::shim::as_arrays(self.remaining());
        if let Some(arrays) = arrays.get(..n) {
            self.by += n as u32 * N as u32; // XXX
            Ok(arrays)
        } else {
            Err(Error {})
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
        }
    }
}
