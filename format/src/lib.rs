// sndjvu_format
// Copyright (C) 2021 Cole Miller
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

#![allow(clippy::from_over_into)]

use bstr::ByteSlice;

#[doc(inline)]
pub use sndjvu_waist::BzzBuffer;

#[derive(Clone, Debug)]
struct Parsing<'raw> {
    location: ParseLocation,
    inner: &'raw [u8],
}

impl<'raw> Parsing<'raw> {
    fn new(raw: &'raw [u8]) -> Self {
        Self {
            location: ParseLocation::InRaw {
                offset: 0,
                path: ParsePath::Root,
            },
            inner: raw,
        }
    }

    fn len(&self) -> usize {
        self.inner.len()
    }

    fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    fn take(&mut self) -> Self {
        let len = self.len();
        let me = self.clone();
        self.advance(len);
        me
    }

    fn advance(&mut self, by: usize) {
        self.location.advance(by);
        self.inner = &self.inner[by..];
    }

    fn advance_to_even(&mut self) {
        match self {
            Self { location: ParseLocation::InRaw { offset, .. }, .. } => {
                if *offset % 2 != 0 && !self.is_empty() {
                    self.advance(1);
                }
            }
            _ => panic!(
                "parser bug: called `Parsing::advance_to_even` on a value of the wrong shape, `{:?}`",
                self
            ),
        }
    }

    fn get_bytes<const N: usize>(&mut self) -> Option<[u8; N]> {
        self.inner.get(0..N).map(|bytes| {
            let mut arr = [0; N];
            arr.copy_from_slice(bytes);
            self.advance(N);
            arr
        })
    }

    fn get_span(&mut self, n: usize) -> Result<&'raw [u8], usize> {
        let len = self.len();
        self.inner
            .get(0..n)
            .map(|bytes| {
                self.advance(n);
                bytes
            })
            .ok_or(len)
    }

    fn split_off(&mut self, n: usize) -> Result<Self, usize> {
        let len = self.len();
        self.inner
            .get(0..n)
            .map(|bytes| {
                let location = self.location;
                self.advance(n);
                Self {
                    location,
                    inner: bytes,
                }
            })
            .ok_or(len)
    }

    fn leaf(self, id: [u8; 4]) -> Self {
        match self {
            Self {
                location:
                    ParseLocation::InRaw {
                        offset,
                        path: ParsePath::Form(parent),
                    },
                inner,
            } => Self {
                location: ParseLocation::InRaw {
                    offset,
                    path: ParsePath::Leaf(LeafPath { parent, id }),
                },
                inner,
            },
            _ => panic!(
                "parser bug: called `Parsing::leaf` on a value of the wrong shape, `{:?}`",
                self
            ),
        }
    }

    fn form(self, kind: [u8; 4]) -> Self {
        match self {
            Self {
                location: ParseLocation::InRaw { offset, path },
                inner,
            } => {
                let path = match path {
                    ParsePath::Root | ParsePath::Form(_) => ParsePath::Form(FormPath { kind }),
                    ParsePath::Leaf(_) => panic!(
                        "parse bug: called `Parsing::form` on a value of the wrong shape, `{:?}`",
                        self
                    ),
                };
                Self {
                    location: ParseLocation::InRaw { offset, path },
                    inner,
                }
            }
            _ => panic!(
                "parse bug: called `Parsing::form` on a value of the wrong shape, `{:?}`",
                self
            ),
        }
    }

    fn get_until_nul(&mut self) -> Option<&'raw [u8]> {
        let nul = self.inner.find_byte(b'\0')?;
        // `find_byte` tells us that there are at least `nul + 1` bytes remaining
        let s = self.get_span(nul).unwrap();
        self.advance(1);
        Some(s)
    }

    fn get_u8(&mut self) -> Option<u8> {
        self.get_bytes::<1>().map(|[x]| x)
    }

    fn get_u8_or_missing_content(&mut self) -> Result<u8, ParseError> {
        self.get_bytes::<1>()
            .map(|[x]| x)
            .ok_or_else(|| ParseError::missing_content(self))
    }

    fn get_u16_be_or_missing_content(&mut self) -> Result<u16, ParseError> {
        self.get_bytes::<2>()
            .map(u16::from_be_bytes)
            .ok_or_else(|| ParseError::missing_content(self))
    }

    fn get_u16_le_or_missing_content(&mut self) -> Result<u16, ParseError> {
        self.get_bytes::<2>()
            .map(u16::from_le_bytes)
            .ok_or_else(|| ParseError::missing_content(self))
    }

    fn get_u24_be(&mut self) -> Option<u32> {
        self.get_bytes::<3>()
            .map(|[x, y, z]| u32::from_be_bytes([0, x, y, z]))
    }

    fn get_u24_be_or_missing_content(&mut self) -> Result<u32, ParseError> {
        self.get_bytes::<3>()
            .map(|[x, y, z]| u32::from_be_bytes([0, x, y, z]))
            .ok_or_else(|| ParseError::missing_content(self))
    }

    fn get_u32_be(&mut self) -> Option<u32> {
        self.get_bytes::<4>().map(u32::from_be_bytes)
    }

    fn bzz(self) -> Bzz<'raw> {
        match self {
            Self {
                location:
                    ParseLocation::InRaw {
                        offset,
                        path: ParsePath::Leaf(path),
                    },
                inner,
            } => Bzz {
                start: offset,
                path,
                raw: inner,
            },
            _ => panic!(
                "parser bug: called `Parsing::bzz` on a value of the wrong shape, `{:?}`",
                self
            ),
        }
    }
}

#[derive(Clone, Debug)]
struct Bzz<'raw> {
    start: usize,
    path: LeafPath,
    raw: &'raw [u8],
}

impl<'raw> Bzz<'raw> {
    fn parsing_decoded<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Parsing<'dec> {
        let Self { start, path, raw } = self;
        let decoded_bytes = buffer.decode_bytes(raw, scratch);
        Parsing {
            location: ParseLocation::InBzz {
                start_of_field: *start,
                offset_within_decoded: 0,
                path: *path,
            },
            inner: decoded_bytes,
        }
    }
}

/// Categories of error that can occur while parsing a DjVu document.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ParseErrorKind {
    ChunkLenMismatch { expected: usize, found: usize },
    ContentLenMismatch { expected: usize, found: usize },
    EmptyComponent,
    EmptyDocument,
    Excess,
    IncompleteChunk,
    IncompleteForm,
    Junk,
    MissingChunk { expected: [u8; 4] },
    MissingContent,
    MissingMagic,
    MissingNul,
    UnexpectedChunk { found: [u8; 4] },
    UnexpectedForm { found: [u8; 4] },
    UnexpectedVal { found: u8 },
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::ChunkLenMismatch { expected, found } => write!(
                f,
                "stated chunk length is {}, but only {} bytes are available",
                expected, found
            ),
            Self::ContentLenMismatch { expected, found } => write!(
                f,
                "stated field length is {}, but only {} bytes are available",
                expected, found
            ),
            Self::EmptyComponent => write!(f, "component is empty (no top-level container)"),
            Self::EmptyDocument => write!(f, "document is empty (no top-level container)"),
            Self::Excess => write!(f, "excess bytes at the end of a chunk"),
            Self::IncompleteChunk => write!(f, "couldn't parse a complete chunk header"),
            Self::IncompleteForm => write!(f, "`FORM` chunk is missing kind bytes"),
            Self::Junk => write!(f, "encountered invalid bytes for chunk ID"),
            Self::MissingChunk { expected } => {
                write!(f, "required `{}` chunk is missing", expected.as_bstr())
            }
            Self::MissingContent => write!(f, "data is missing a field"),
            Self::MissingMagic => write!(f, "document is missing the DjVu magic bytes"),
            Self::MissingNul => write!(f, "string is missing terminating NUL byte"),
            Self::UnexpectedChunk { found } => {
                write!(f, "didn't expect a `{}` chunk here", found.as_bstr())
            }
            Self::UnexpectedForm { found } => {
                write!(f, "didn't expect a `FORM:{}` chunk here", found.as_bstr())
            }
            Self::UnexpectedVal { found } => {
                write!(f, "the value {:#02x} isn't allowed for this field", found)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FormPath {
    kind: [u8; 4],
}

impl std::fmt::Display for FormPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "/FORM:{}", self.kind.as_bstr())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct LeafPath {
    parent: FormPath,
    id: [u8; 4],
}

impl std::fmt::Display for LeafPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.parent, self.id.as_bstr())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ParsePath {
    Root,
    Form(FormPath),
    Leaf(LeafPath),
}

impl std::fmt::Display for ParsePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Root => write!(f, "/"),
            Self::Form(path) => write!(f, "{}", path),
            Self::Leaf(path) => write!(f, "{}", path),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ParseLocation {
    InRaw {
        offset: usize,
        path: ParsePath,
    },
    InBzz {
        start_of_field: usize,
        offset_within_decoded: usize,
        path: LeafPath,
    },
}

impl ParseLocation {
    fn advance(&mut self, by: usize) {
        match self {
            Self::InRaw { offset, .. } => *offset += by,
            Self::InBzz {
                offset_within_decoded,
                ..
            } => *offset_within_decoded += by,
        }
    }
}

impl std::fmt::Display for ParseLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::InRaw { offset, path } => {
                write!(f, "offset {}, path {}", offset, path)?;
            }
            Self::InBzz {
                start_of_field,
                offset_within_decoded,
                path,
            } => {
                write!(
                    f,
                    "offset {}bzz+{}, path {}",
                    start_of_field, offset_within_decoded, path
                )?;
            }
        }
        Ok(())
    }
}

/// An error encountered while trying to parse part of a DjVu document.
///
/// This error type is returned by most fallible functions in this library. An exception is
/// [`Annotations::try_next`](crate::ant::Annotations::try_next), which has its own error type,
/// [`AnnotationError`](crate::ant::AnnotationError).
#[derive(Clone, Debug, Hash)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    location: ParseLocation,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {}: {}", self.location, self.kind)
    }
}

impl std::error::Error for ParseError {}

impl ParseError {
    fn new(parsing: &Parsing<'_>, kind: ParseErrorKind) -> Self {
        Self {
            kind,
            location: parsing.location,
        }
    }

    fn incomplete_chunk(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::IncompleteChunk)
    }

    fn chunk_len_mismatch(parsing: &Parsing<'_>, expected: usize, found: usize) -> Self {
        Self::new(
            parsing,
            ParseErrorKind::ChunkLenMismatch { expected, found },
        )
    }

    fn missing_content(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::MissingContent)
    }

    fn incomplete_form(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::IncompleteForm)
    }

    fn junk(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::Junk)
    }

    fn excess(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::Excess)
    }

    fn content_len_mismatch(parsing: &Parsing<'_>, expected: usize, found: usize) -> Self {
        Self::new(
            parsing,
            ParseErrorKind::ContentLenMismatch { expected, found },
        )
    }

    fn unexpected_chunk(parsing: &Parsing<'_>, found: [u8; 4]) -> Self {
        Self::new(parsing, ParseErrorKind::UnexpectedChunk { found })
    }

    fn unexpected_form(parsing: &Parsing<'_>, found: [u8; 4]) -> Self {
        Self::new(parsing, ParseErrorKind::UnexpectedForm { found })
    }

    fn missing_chunk(parsing: &Parsing<'_>, expected: [u8; 4]) -> Self {
        Self::new(parsing, ParseErrorKind::MissingChunk { expected })
    }

    fn missing_nul(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::MissingNul)
    }

    fn unexpected_val(parsing: &Parsing<'_>, found: u8) -> Self {
        Self::new(parsing, ParseErrorKind::UnexpectedVal { found })
    }

    fn empty_component(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::EmptyComponent)
    }

    fn empty_document(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::EmptyDocument)
    }

    fn missing_magic(parsing: &Parsing<'_>) -> Self {
        Self::new(parsing, ParseErrorKind::MissingMagic)
    }
}

pub use crate::document::parse_document;
pub use crate::multi_page::parse_component;

pub mod ant;
mod chunk;
pub mod document;
pub mod multi_page;
pub mod page;
pub mod txt;
