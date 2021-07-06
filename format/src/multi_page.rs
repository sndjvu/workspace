// sndjvu_format::multi_page
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

use crate::chunk::*;
use crate::page::{Iw44, Page, PageElements};
use crate::{Bzz, BzzBuffer, ParseError, Parsing};
use bstr::{BStr, ByteSlice};
#[allow(unused)]
use std::convert::{TryFrom, TryInto};

/// The version metadata associated with a multi-page DjVu document.
///
/// Call [`MultiPageVersion::into`] to get the contained `u8` value.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MultiPageVersion(u8);

impl std::fmt::Display for MultiPageVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl MultiPageVersion {
    /// The latest version implemented by this library.
    pub const CURRENT: Self = Self(1);
}

impl TryFrom<u8> for MultiPageVersion {
    type Error = ();

    fn try_from(x: u8) -> Result<Self, Self::Error> {
        if x & (1 << 7) == 0 {
            Ok(Self(x))
        } else {
            Err(())
        }
    }
}

impl Into<u8> for MultiPageVersion {
    fn into(self) -> u8 {
        self.0
    }
}

/// The kinds of component that may be present in a multi-page DjVu file.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ComponentKind {
    Bundle,
    Page,
    Thumbnails,
}

/// Metadata about a component of a multi-page DjVu file.
#[derive(Clone, Debug)]
pub struct ComponentMeta<'dec> {
    pub kind: ComponentKind,
    pub size: u32,
    pub id: &'dec BStr,
    pub name: Option<&'dec BStr>,
    pub title: Option<&'dec BStr>,
}

impl<'dec> ComponentMeta<'dec> {
    fn parse_from(
        mut parsing: Parsing<'dec>,
        num_components: u16,
    ) -> Result<Vec<Self>, ParseError> {
        let init_meta = ComponentMeta {
            kind: ComponentKind::Bundle,
            size: Default::default(),
            id: b"".as_bstr(),
            name: None,
            title: None,
        };
        let mut meta = vec![init_meta; num_components as usize];

        for entry in &mut meta {
            entry.size = parsing.get_u24_be_or_missing_content()?;
        }
        for entry in &mut meta {
            let flags = parsing.get_u8_or_missing_content()?;
            if flags & (1 << 7) != 0 {
                entry.name = Some(b"".as_bstr());
            }
            if flags & (1 << 6) != 0 {
                entry.title = Some(b"".as_bstr());
            }
            entry.kind = match flags & 0b111111 {
                0 => ComponentKind::Bundle,
                1 => ComponentKind::Page,
                2 => ComponentKind::Thumbnails,
                _ => return Err(ParseError::unexpected_val(&parsing, flags)),
            }
        }
        for entry in &mut meta {
            entry.id = parsing
                .get_until_nul()
                .ok_or_else(|| ParseError::missing_nul(&parsing))?
                .as_bstr();
            if entry.name.is_some() {
                let name = parsing
                    .get_until_nul()
                    .ok_or_else(|| ParseError::missing_nul(&parsing))?
                    .as_bstr();
                entry.name = Some(name);
            }
            if entry.title.is_some() {
                let title = parsing
                    .get_until_nul()
                    .ok_or_else(|| ParseError::missing_nul(&parsing))?
                    .as_bstr();
                entry.title = Some(title);
            }
        }
        Ok(meta)
    }
}

/// A single offset parsed from a `DIRM` chunk.
///
/// You can’t do much with a value of this type, except convert it into a `u32` with
/// [`ComponentOffset::into`].
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct ComponentOffset([u8; 4]);

impl Into<u32> for ComponentOffset {
    fn into(self) -> u32 {
        u32::from_be_bytes(self.0)
    }
}

fn offsets_from_bytes(bytes: &[u8]) -> &[ComponentOffset] {
    assert!(bytes.len() % 4 == 0);
    let ptr = bytes.as_ptr().cast();
    let len = bytes.len() / 4;
    // SAFETY ComponentOffset is repr(transparent), bytes.len() is a multiple of 4, and [u8; 4] has
    // the same alignment as u8
    unsafe { std::slice::from_raw_parts(ptr, len) }
}

/// Data specific to bundled [`MultiPage`] documents: the document components and their offsets.
#[derive(Clone, Debug)]
pub struct Bundled<'raw> {
    pub offsets: &'raw [ComponentOffset],
    chunks: FormChunks<'raw>,
}

impl<'raw> Bundled<'raw> {
    /// Returns a fallible iterator that will attempt to parse the [`Component`]s in this document.
    pub fn components(&self) -> BundledComponents<'raw> {
        BundledComponents {
            chunks: self.chunks.clone(),
        }
    }
}

/// Metadata about the components of a [`MultiPage`] document, BZZ-encoded.
#[derive(Clone, Debug)]
pub struct EncodedMeta<'raw> {
    num_components: u16,
    bzz: Bzz<'raw>,
}

impl<'raw> EncodedMeta<'raw> {
    pub fn decode_and_parse<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Result<Vec<ComponentMeta<'dec>>, ParseError> {
        let Self {
            num_components,
            bzz,
        } = self;
        let parsing = bzz.parsing_decoded(buffer, scratch);
        ComponentMeta::parse_from(parsing, *num_components)
    }
}

/// The outline of a [`MultiPage`] document (`NAVM` chunk), BZZ-encoded.
#[derive(Clone, Debug)]
pub struct EncodedOutline<'raw> {
    bzz: Bzz<'raw>,
}

impl<'raw> EncodedOutline<'raw> {
    pub fn decode_and_parse<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Result<Outline<'dec>, ParseError> {
        let mut parsing = self.bzz.parsing_decoded(buffer, scratch);
        let num_bookmarks = parsing.get_u16_be_or_missing_content()?;
        Ok(Outline {
            num_remaining: num_bookmarks,
            parsing,
        })
    }
}

/// A multi-page DjVu document.
#[derive(Clone, Debug)]
pub struct MultiPage<'raw> {
    pub version: MultiPageVersion,
    pub num_components: u16,
    pub bundled: Option<Bundled<'raw>>,
    pub meta: EncodedMeta<'raw>,
    pub outline: Option<EncodedOutline<'raw>>,
}

impl<'raw> MultiPage<'raw> {
    pub(crate) fn parse_from(mut chunks: LeafChunks<'raw>) -> Result<Self, ParseError> {
        let first = match chunks.try_next() {
            Ok(None) => return Err(ParseError::missing_chunk(&chunks.parsing, *b"DIRM")),
            Ok(Some(chunk)) => chunk,
            Err(e) => return Err(e),
        };
        if &first.id != b"DIRM" {
            return Err(ParseError::unexpected_chunk(&first.content, first.id));
        }
        let mut dirm = first.content;
        let flags = dirm.get_u8_or_missing_content()?;
        let is_bundled = flags & (1 << 7) != 0;
        let version = MultiPageVersion(flags & !(1 << 7));
        let num_components = dirm.get_u16_be_or_missing_content()?;
        let bundled_offsets = if is_bundled {
            let bytes = dirm
                .get_span(num_components as usize * 4)
                .map_err(|found| {
                    ParseError::content_len_mismatch(&dirm, num_components as usize * 4, found)
                })?;
            Some(offsets_from_bytes(bytes))
        } else {
            None
        };
        let meta = EncodedMeta {
            num_components,
            bzz: dirm.bzz(),
        };

        let outline = if chunks.peek_id() == Some(*b"NAVM") {
            // from `peek_id` we know that there are bytes remaining, so if `try_next` succeeds
            // then it must have found a `NAVM`
            let navm = chunks.try_next()?.unwrap();
            Some(EncodedOutline {
                bzz: navm.content.bzz(),
            })
        } else {
            None
        };

        let bundled = bundled_offsets.map(|offsets| Bundled {
            offsets,
            chunks: chunks.into_forms(),
        });
        Ok(Self {
            version,
            num_components,
            bundled,
            meta,
            outline,
        })
    }
}

/// A single bookmark parsed from a document [`Outline`].
#[derive(Clone, Debug)]
pub struct Bookmark<'dec> {
    pub num_children: u8,
    pub description: &'dec BStr,
    pub url: &'dec BStr,
}

/// A fallible iterator that parses successive [`Bookmark`]s from a decoded document outline.
pub struct Outline<'dec> {
    num_remaining: u16,
    parsing: Parsing<'dec>,
}

impl<'dec> Outline<'dec> {
    pub fn is_empty(&self) -> bool {
        self.num_remaining == 0
    }

    pub fn expected_len(&self) -> u16 {
        self.num_remaining
    }

    pub fn try_next(&mut self) -> Result<Option<Bookmark<'dec>>, ParseError> {
        if self.num_remaining == 0 {
            return Ok(None);
        }
        let num_children = self.parsing.get_u8_or_missing_content()?;
        let len_description = self.parsing.get_u24_be_or_missing_content()?;
        let description = self
            .parsing
            .get_span(len_description as usize)
            .map_err(|found| {
                ParseError::content_len_mismatch(&self.parsing, len_description as usize, found)
            })?
            .as_bstr();
        let len_url = self.parsing.get_u24_be_or_missing_content()?;
        let url = self
            .parsing
            .get_span(len_url as usize)
            .map_err(|found| {
                ParseError::content_len_mismatch(&self.parsing, len_url as usize, found)
            })?
            .as_bstr();
        self.num_remaining -= 1;
        Ok(Some(Bookmark {
            num_children,
            description,
            url,
        }))
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'raw> fallible_iterator::FallibleIterator for Outline<'raw> {
    type Item = Bookmark<'raw>;
    type Error = ParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}

#[derive(Clone, Debug)]
pub struct Thumbnails<'raw> {
    chunks: LeafChunks<'raw>,
}

impl<'raw> Thumbnails<'raw> {
    pub fn try_next(&mut self) -> Result<Option<Iw44<'raw>>, ParseError> {
        match self.chunks.try_next() {
            Ok(None) => Ok(None),
            Ok(Some(chunk)) => match &chunk.id {
                b"TH44" => Ok(Some(Iw44(chunk))),
                _ => Err(ParseError::unexpected_chunk(&chunk.content, chunk.id)),
            },
            Err(e) => Err(e),
        }
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'raw> fallible_iterator::FallibleIterator for Thumbnails<'raw> {
    type Item = Iw44<'raw>;
    type Error = ParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}

/// A component of a multi-page DjVu document.
#[derive(Clone, Debug)]
pub enum Component<'raw> {
    /// A `FORM:DJVI` component.
    Bundle(PageElements<'raw>),
    /// A `FORM:DJVU` component.
    Page(Page<'raw>),
    /// A `FORM:THUM` component.
    Thumbnails(Thumbnails<'raw>),
}

impl<'raw> Component<'raw> {
    pub fn kind(&self) -> ComponentKind {
        match self {
            Self::Page(_) => ComponentKind::Page,
            Self::Bundle(_) => ComponentKind::Bundle,
            Self::Thumbnails(_) => ComponentKind::Thumbnails,
        }
    }
}

/// Begin parsing a [`Component`] from some bytes.
///
/// This function is one of two main entry points to this library, the other being
/// [`parse_document`](crate::document::parse_document). You can use this function to parse a
/// component that’s stored in its own file as part of an indirect multi-page document.
pub fn parse_component(raw: &[u8]) -> Result<Component<'_>, ParseError> {
    let mut components = BundledComponents {
        chunks: FormChunks {
            parsing: Parsing::new(raw),
        },
    };
    components
        .try_next()?
        .ok_or_else(|| ParseError::empty_component(&components.chunks.parsing))
}

/// A fallible iterator that parses successive [`Component`]s from a bundled [`MultiPage`]
/// document.
pub struct BundledComponents<'raw> {
    chunks: FormChunks<'raw>,
}

impl<'raw> BundledComponents<'raw> {
    pub fn try_next(&mut self) -> Result<Option<Component<'raw>>, ParseError> {
        match self.chunks.try_next() {
            Ok(None) => Ok(None),
            Ok(Some(form)) => match &form.kind {
                b"DJVI" => Ok(Some(Component::Bundle(PageElements {
                    chunks: form.leaves(),
                }))),
                b"DJVU" => {
                    let page = Page::parse_from(form.leaves())?;
                    Ok(Some(Component::Page(page)))
                }
                b"THUM" => Ok(Some(Component::Thumbnails(Thumbnails {
                    chunks: form.leaves(),
                }))),
                _ => Err(ParseError::unexpected_form(&form.content, form.kind)),
            },
            Err(e) => Err(e),
        }
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'raw> fallible_iterator::FallibleIterator for BundledComponents<'raw> {
    type Item = Component<'raw>;
    type Error = ParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}
