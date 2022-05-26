/// The outcome of a parsing operation, if no [`Error`] was encountered.
pub enum Progress<T, D = Void> {
    /// Not enough data was presented to complete the parsing operation.
    None {
        /// If [`Some`], suggests how many more bytes might be needed to advance.
        hint: Option<usize>,
    },
    /// Parsing was successful.
    Advanced {
        /// The parsed object.
        head: T,
        /// How many bytes were consumed.
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
/// This will become an alias for the never type `!` once that's stabilized.
pub enum Void {}

fn advanced<T, D>(head: T, s: &Split<'_>) -> Progress<T, D> {
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
    let field = Field::new(data, 0);
    let mut s = field.split();
    let s = &mut s;
    let (kind, len) = try_advance!(magic_form_header(s)?);
    let head = match &kind {
        b"DJVU" => {
            let info = try_advance!(get_info_chunk(s)?);
            let elements = ElementP::mark(s, len);
            DocumentHead::SinglePage { info, elements }
        }
        b"DJVM" => {
            let content = try_advance!(specific_chunk(s, b"DIRM")?);
            let dirm = Dirm::parse(content)?;
            let kind = try_advance!(peek_chunk(s)?);
            let navm = if &kind == b"NAVM" {
                let content = try_advance!(specific_chunk(s, b"NAVM")?);
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
    todo!()
}

/// Parsed representation of the start of a document.
pub enum DocumentHead<'a> {
    SinglePage {
        info: Info<'a>,
        /// "Pointer" to the first element.
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
        let info = (|| {
            let width = s.u16_be()?;
            let height = s.u16_be()?;
            let &[minor, major] = s.byte_array()?;
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
            Some(Self {
                content,
                width,
                height,
                version: crate::InfoVersion { major, minor },
                dpi,
                gamma,
                rotation,
            })
        })().ok_or(Error {})?;
        Ok(info)
    }
}

fn is_potential_chunk_id(xs: [u8; 4]) -> bool {
    xs.iter().all(u8::is_ascii_alphanumeric)
}

fn get_info_chunk<'a>(s: &mut Split<'a>) -> Result<Progress<Info<'a>>, Error> {
    let (kind, len) = try_advance!(chunk_header(s)?);
    if &kind != b"INFO" {
        return Err(Error {});
    }
    let content = try_advance!(chunk_content(s, len));
    let info = Info::parse(content)?;
    Ok(advanced(info, s))
}

/// "Pointer" to an [`Element`] within a document.
pub struct ElementP {
    pos: Pos,
    end_pos: Pos,
}

impl ElementP {
    fn mark(s: &Split<'_>, len: u32) -> Self {
        Self {
            pos: s.pos(),
            end_pos: s.pos() + len,
        }
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<Element<'a>, ()>, Error> {
        todo!()
    }

    pub fn is_end(&self) -> bool {
        self.pos == self.end_pos
    }
}

pub enum Element<'a> {
    Incl(Incl<'a>),
}

impl<'a> Element<'a> {
    pub fn after(&self) -> ElementP {
        todo!()
    }
}

pub struct Incl<'a> {
    content: Field<'a>,
}

pub struct Dirm<'a> {
    content: Field<'a>,
    pub version: crate::DirmVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'a>>,
    bzz: Field<'a>,
}

impl<'a> Dirm<'a> {
    fn parse(content: Field<'a>) -> Result<Self, Error> {
        let mut s = content.split();
        let dirm = (|| {
            let flags = s.byte()?;
            let is_bundled = flags & (1 << 7) != 0;
            let version = crate::DirmVersion(flags & !(1 << 7));
            let num_components = s.u16_be()?;
            let bundled = if is_bundled {
                let arrays = s.slice_of_arrays(num_components as usize)?;
                let offsets = ComponentOffset::cast_slice(arrays);
                Some(Bundled { offsets })
            } else {
                None
            };
            let bzz = s.rest();
            Some(Dirm {
                content,
                version,
                num_components,
                bundled,
                bzz,
            })
        })().ok_or(Error {})?;
        Ok(dirm)
    }
}

pub struct Bundled<'a> {
    offsets: &'a [ComponentOffset],
}

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
}

impl<'a> Bundled<'a> {
    pub fn get(&self, i: usize) -> Option<ComponentP> {
        todo!()
    }

    pub fn iter(&self) -> BundledIter<'a> {
        todo!()
    }
}

pub struct BundledIter<'a> {
    offsets: core::slice::Iter<'a, ComponentOffset>,
}

impl<'a> Iterator for BundledIter<'a> {
    type Item = ComponentP;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.offsets.size_hint()
    }
}

impl<'a> core::iter::ExactSizeIterator for BundledIter<'a> {}
impl<'a> core::iter::FusedIterator for BundledIter<'a> {}

impl<'a> core::iter::DoubleEndedIterator for BundledIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct Navm<'a> {
    content: Field<'a>,
}

pub struct ComponentP {
}

impl ComponentP {
    pub fn offset(&self) -> u32 {
        todo!()
    }

    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<ComponentHead<'a>, ()>, Error> {
        todo!()
    }
}

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

pub struct ThumbnailP {
}

impl ThumbnailP {
    pub fn feed<'a>(&self, data: &'a [u8]) -> Result<Progress<Thumbnail<'a>, ()>, Error> {
        todo!()
    }

    pub fn is_end(&self) -> bool {
        todo!()
    }
}

pub struct Thumbnail<'a> {
    content: Field<'a>,
}

impl<'a> Thumbnail<'a> {
    pub fn after(&self) -> ThumbnailP {
        todo!()
    }
}

type Pos = u32;

#[derive(Clone, Copy, Debug)]
struct Field<'a> {
    full: &'a [u8],
    start: usize,
    start_pos: Pos,
    end: usize,
}

impl<'a> Field<'a> {
    fn new(full: &'a [u8], start_pos: Pos) -> Self {
        Self {
            full,
            start: 0,
            start_pos,
            end: full.len(),
        }
    }

    fn split(self) -> Split<'a> {
        Split {
            parent: self,
            by: 0,
        }
    }
}

struct Split<'a> {
    parent: Field<'a>,
    by: u32,
}

impl<'a> Split<'a> {
    fn pos(&self) -> Pos {
        self.parent.start_pos + self.by
    }

    fn remaining(&self) -> &'a [u8] {
        &self.parent.full[self.parent.start + self.by as usize..self.parent.end]
    }

    fn field(&mut self, len: u32) -> Option<Field<'a>> {
        todo!()
    }

    fn rest(self) -> Field<'a> {
        todo!()
    }

    fn byte_array<const N: usize>(&mut self) -> Option<&'a [u8; N]> {
        if let Some((arr, _)) = crate::shim::split_array(self.remaining()) {
            self.by += N as u32; // ugh
            Some(arr)
        } else {
            None
        }
    }

    fn slice_of_arrays<const N: usize>(&mut self, n: usize) -> Option<&'a [[u8; N]]> {
        let (all, _) = crate::shim::as_arrays(self.remaining());
        if let Some(arrays) = all.get(..n) {
            self.by += n as u32 * N as u32; // UGH
            Some(arrays)
        } else {
            None
        }
    }

    fn byte(&mut self) -> Option<u8> {
        let &[x] = self.byte_array()?;
        Some(x)
    }

    fn u16_be(&mut self) -> Option<u16> {
        let &xs = self.byte_array()?;
        Some(u16::from_be_bytes(xs))
    }

    fn u16_le(&mut self) -> Option<u16> {
        let &xs = self.byte_array()?;
        Some(u16::from_le_bytes(xs))
    }

    fn u32_be(&mut self) -> Option<u32> {
        let &xs = self.byte_array()?;
        Some(u32::from_be_bytes(xs))
    }
}

fn chunk_header(s: &mut Split<'_>) -> Result<Progress<([u8; 4], u32)>, Error> {
    let got = (|| {
        let &kind = s.byte_array()?;
        let len = s.u32_be()?;
        Some((kind, len))
    })();
    let (kind, len) = match got {
        None => return Ok(Progress::None { hint: Some(8) }),
        Some((kind, len)) => (kind, len),
    };
    if !is_potential_chunk_id(kind) {
        return Err(Error {});
    }
    Ok(Progress::Advanced {
        head: (kind, len),
        by: 8,
    })
}

fn chunk_content<'a>(s: &mut Split<'a>, len: u32) -> Progress<Field<'a>> {
    if let Some(field) = s.field(len) {
        advanced(field, s)
    } else {
        Progress::None { hint: Some(len as usize) }
    }
}

fn magic_form_header(s: &mut Split<'_>) -> Result<Progress<([u8; 4], u32)>, Error> {
    todo!()
}

fn specific_chunk<'a>(s: &mut Split<'a>, kind: &[u8; 4]) -> Result<Progress<Field<'a>>, Error> {
    todo!()
}

fn peek_chunk(s: &Split<'_>) -> Result<Progress<[u8; 4]>, Error> {
    todo!()
}
