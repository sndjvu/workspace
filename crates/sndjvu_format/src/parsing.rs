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

// workaround for unstable ? overloading (Try trait)
macro_rules! try_advance {
    ( $x:expr ) => {
        match $x {
            Progress::None { hint } => return Ok(Progress::None { hint }),
            Progress::Advanced { head, .. } => head,
            Progress::End(d) => match d {},
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
            let info_content = try_advance!(s.specific_chunk(b"INFO")?);
            let info = Info::parse(info_content)?;
            let elements = ElementP::new(s.pos(), end_pos);
            DocumentHead::SinglePage { info, elements }
        }
        b"DJVM" => {
            let dirm_content = try_advance!(s.specific_chunk(b"DIRM")?);
            let dirm = Dirm::parse(dirm_content, end_pos)?;
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
    s.component_head(kind, len)
}

/// Parsed representation of the start of a document.
pub enum DocumentHead<'a> {
    SinglePage {
        info: Info<'a>,
        elements: ElementP,
    },
    MultiPage {
        /// The parsed `DIRM` chunk.
        ///
        /// You can obtain [`ComponentP`] objects for each component from this type,
        /// if the document is bundled.
        dirm: Dirm<'a>,
        /// The parsed `NAVM` chunk.
        navm: Option<Navm<'a>>,
    },
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

impl<'a> Info<'a> {
    fn parse(content: Field<'a>) -> Result<Self, Error> {
        let mut s = content.split();
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
        Ok(Self {
            content,
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
    Incl(Incl<'a>),
}

impl<'a> Element<'a> {
    pub fn after(&self) -> ElementP {
        match *self {
            Self::Incl(Incl { after_pos, end_pos, .. }) => ElementP::new(after_pos, end_pos),
        }
    }
}

pub struct Incl<'a> {
    content: Field<'a>,
    after_pos: Pos,
    end_pos: Pos,
}

pub struct Dirm<'a> {
    content: Field<'a>,
    pub version: crate::DirmVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'a>>,
    bzz: Field<'a>,
}

impl<'a> Dirm<'a> {
    fn parse(content: Field<'a>, end_pos: Pos) -> Result<Self, Error> {
        let mut s = content.split();
        let flags = s.byte()?;
        let is_bundled = flags >> 7 != 0;
        let version = crate::DirmVersion(flags & 0b0111_1111);
        let num_components = s.u16_be()?;
        let bundled = if is_bundled {
            let arrays = s.slice_of_arrays(num_components as usize)?;
            let offsets = ComponentOffset::cast_slice(arrays);
            Some(Bundled { offsets, end_pos })
        } else {
            None
        };
        let bzz = s.rest();
        Ok(Dirm {
            content,
            version,
            num_components,
            bundled,
            bzz,
        })
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
    pub fn get(&self, i: usize) -> Option<ComponentP> {
        self.offsets.get(i).map(|off| ComponentP::new(off.get(), self.end_pos))
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
        info: Info<'a>,
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

    fn header<const N: usize>(&mut self) -> Option<&'a [[u8; 4]; N]> {
        todo!()
    }

    fn field(&mut self, len: u32) -> Option<Field<'a>> {
        todo!()
    }

    fn align(&mut self) -> Progress<()> {
        let mut padding = 0;
        if self.pos() % 2 != 0 {
            if self.end_pos == Some(self.pos()) {
                return Progress::None { hint: None };
            }
            padding = 1;
        }
        self.by += padding;
        Progress::Advanced { head: (), by: padding as usize }
    }

    fn form_header(&mut self) -> Result<Progress<([u8; 4], u32)>, Error> {
        let padding = match self.align() {
            Progress::None { .. } => return Ok(Progress::None { hint: None }), // FIXME
            Progress::Advanced { by, .. } => by,
            Progress::End(d) => match d {},
        };
        let &[id, xs, kind] = match self.header() {
            None => return Ok(Progress::None { hint: None }), // FIXME
            Some(got) => got,
        };
        let len = u32::from_be_bytes(xs);
        if &id != b"FORM" {
            return Err(Error {});
        }
        if !is_potential_chunk_id(kind) {
            return Err(Error {});
        }
        Ok(Progress::Advanced { head: (kind, len - 4), by: padding + 3 * 4 })
    }

    fn magic_form_header(&mut self) -> Result<Progress<([u8; 4], u32)>, Error> {
        // no need to align here, magic number always occurs at the very beginning
        let &[magic, id, xs, kind] = match self.header() {
            None => return Ok(Progress::None { hint: None }), // FIXME
            Some(got) => got,
        };
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
        Ok(Progress::Advanced { head: (kind, len - 4), by: 4 * 4 })
    }

    fn chunk(&mut self) -> Result<Progress<([u8; 4], Field<'a>)>, Error> {
        let padding = match self.align() {
            Progress::None { .. } => return Ok(Progress::None { hint: None }), // FIXME
            Progress::Advanced { by, .. } => by,
            Progress::End(d) => match d {},
        };
        let &[id, xs] = match self.header() {
            None => return Ok(Progress::None { hint: None }), // FIXME
            Some(got) => got,
        };
        let len = u32::from_be_bytes(xs);
        if !is_potential_chunk_id(id) {
            return Err(Error {});
        }
        let content = match self.field(len) {
            None => return Ok(Progress::None { hint: None }), // FIXME
            Some(got) => got,
        };
        Ok(Progress::Advanced { head: (id, content), by: padding + 2 * 4 + len as usize })
    }

    fn specific_chunk(&mut self, expected: &[u8; 4]) -> Result<Progress<Field<'a>>, Error> {
        let progress = match self.chunk()? {
            Progress::None { hint } => Progress::None { hint },
            Progress::Advanced { head: (id, content), by } => {
                if &id != expected {
                    return Err(Error {});
                }
                Progress::Advanced { head: content, by }
            }
            Progress::End(d) => match d {},
        };
        Ok(progress)
    }

    fn peek_chunk(&self) -> Result<Progress<[u8; 4]>, Error> {
        let mut s = self.clone();
        match s.align() {
            Progress::None { .. } => return Ok(Progress::None { hint: None }), // FIXME
            Progress::Advanced { .. } => {},
            Progress::End(d) => match d {},
        }
        let id = match s.header() {
            None => return Ok(Progress::None { hint: None }), // FIXME
            Some(&[got]) => got,
        };
        if !is_potential_chunk_id(id) {
            return Err(Error {});
        }
        Ok(Progress::Advanced { head: id, by: 0 })
    }

    fn component_head(&mut self, kind: [u8; 4], len: u32) -> Result<Progress<ComponentHead<'a>>, Error> {
        let end_pos = self.set_distance_to_end(len);
        let head = match &kind {
            b"DJVI" => { 
                let elements = ElementP::new(self.pos(), end_pos);
                ComponentHead::Djvi { elements }
            }
            b"DJVU" => {
                let info_content = try_advance!(self.specific_chunk(b"INFO")?);
                let info = Info::parse(info_content)?;
                let elements = ElementP::new(self.pos(), end_pos);
                ComponentHead::Djvu { info, elements }
            }
            b"THUM" => {
                let thumbnails = ThumbnailP::new(self.pos(), end_pos);
                ComponentHead::Thum { thumbnails }
            }
            _ => return Err(Error {}),
        };
        // can't use fn advanced here
        todo!()
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

    fn rest(self) -> Field<'a> {
        Field {
            full: self.parent.full,
            start: self.parent.start + self.by as usize,
            start_pos: self.parent.start_pos + self.by,
            end: self.parent.end,
        }
    }
}
