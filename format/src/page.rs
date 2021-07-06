// sndjvu_format::page
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

use crate::ant::Ant;
use crate::chunk::*;
use crate::txt::Txt;
use crate::{Bzz, BzzBuffer, ParseError, Parsing};
use bstr::{BStr, ByteSlice};
use rgb::{alt::BGR8, AsPixels};
use std::cmp::Ordering;
#[allow(unused)]
use std::convert::{TryFrom, TryInto};
use std::num::NonZeroU8;

pub use crate::chunk::Leaf;

/// The version metadata associated with a DjVu page.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PageVersion {
    pub major: u8,
    pub minor: u8,
}

impl std::fmt::Display for PageVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

impl PageVersion {
    /// The most recent version implemented by this library.
    pub const CURRENT: Self = Self {
        major: 0,
        minor: 26,
    };
}

/// The rotation metadata associated with a DjVu page.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum Rotation {
    /// “Right side up”.
    Up = 1,
    /// “90&deg; counterclockwise”.
    Ccw = 6,
    /// “Up side down”.
    Down = 2,
    /// “90&deg; clockwise”.
    Cw = 5,
}

impl Default for Rotation {
    fn default() -> Self {
        Self::Up
    }
}

/// A data element within a DjVu page, representing a single [`Leaf`] chunk.
#[derive(Clone, Debug)]
pub enum Element<'raw> {
    Ant(Ant<'raw>),
    Txt(Txt<'raw>),
    Djbz(Djbz<'raw>),
    Sjbz(Sjbz<'raw>),
    Fg44(Iw44<'raw>),
    Bg44(Iw44<'raw>),
    Fgbz(Fgbz<'raw>),
    Incl(Incl<'raw>),
    Bgjp(Jpeg<'raw>),
    Fgjp(Jpeg<'raw>),
    Smmr(Smmr<'raw>),
    /// A well-formed chunk that wasn’t recognized as any of the above.
    ///
    /// Such a chunk might be present because the document reflects an earlier version of the DjVu
    /// standard that’s not implemented by this library.
    Unrecognized(Leaf<'raw>),
}

/// An `INCL` element.
#[derive(Clone, Debug)]
pub struct Incl<'raw>(Leaf<'raw>);

impl<'raw> Incl<'raw> {
    /// Returns the ID of the `DJVI` component pointed to by this element.
    pub fn target_id(&self) -> &'raw BStr {
        self.0.content_bytes().as_bstr()
    }
}

/// A `Djbz` element.
#[derive(Clone, Debug)]
pub struct Djbz<'raw>(Leaf<'raw>);

impl<'raw> Djbz<'raw> {
    /// Returns the raw bytes contained in this chunk.
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }
}

/// An `Sjbz` element.
#[derive(Clone, Debug)]
pub struct Sjbz<'raw>(Leaf<'raw>);

impl<'raw> Sjbz<'raw> {
    /// Returns the raw bytes contained in this chunk.
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }
}

/// The version metadata associated with an [`Fgbz`] element.
///
/// Call [`PaletteVersion::into`] to get the contained `u8` value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct PaletteVersion(u8);

impl std::fmt::Display for PaletteVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TryFrom<u8> for PaletteVersion {
    type Error = ();

    fn try_from(x: u8) -> Result<Self, Self::Error> {
        if x & (1 << 7) == 0 {
            Ok(Self(x))
        } else {
            Err(())
        }
    }
}

impl Into<u8> for PaletteVersion {
    fn into(self) -> u8 {
        self.0
    }
}

impl PaletteVersion {
    /// The most recent version implemented by this library.
    pub const CURRENT: Self = Self(0);
}

/// An `FGbz` element.
#[derive(Clone, Debug)]
pub struct Fgbz<'raw>(Leaf<'raw>);

impl<'raw> Fgbz<'raw> {
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }

    /// Attempts to parse the raw content of this chunk.
    pub fn parse(&self) -> Result<Palette<'raw>, ParseError> {
        Palette::parse_from(self.0.content.clone())
    }
}

/// Shape-table correspondence data in a [`Palette`], BZZ-encoded.
#[derive(Clone, Debug)]
pub struct EncodedIndices<'raw> {
    /// The *stated* number of indices in the correspondence data.
    ///
    /// In a malformed document, the decoded data may contain more or less than the number of bytes
    /// implied by this field, in which case you’ll get a parse error when decoding.
    pub expected_len: u32,
    bzz: Bzz<'raw>,
}

impl<'raw> EncodedIndices<'raw> {
    pub fn decode_and_parse<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Result<&'dec [PaletteIndex], ParseError> {
        let parsing = self.bzz.parsing_decoded(buffer, scratch);
        PaletteIndex::parse_from(parsing, self.expected_len)
    }
}

/// Parsed representation of an [`Fgbz`] element.
#[derive(Clone, Debug)]
pub struct Palette<'raw> {
    pub version: PaletteVersion,
    pub entries: &'raw [BGR8],
    /// Shape-table correspondence data.
    pub indices: Option<EncodedIndices<'raw>>,
}

impl<'raw> Palette<'raw> {
    fn parse_from(mut parsing: Parsing<'raw>) -> Result<Self, ParseError> {
        let flags = parsing.get_u8_or_missing_content()?;
        let has_indices = flags & (1 << 7) != 0;
        let version = PaletteVersion(flags & !(1 << 7));
        let num_entries = parsing.get_u16_be_or_missing_content()?;
        let entries: &[BGR8] = parsing
            .get_span(num_entries as usize * 3)
            .map_err(|found| {
                ParseError::content_len_mismatch(&parsing, num_entries as usize * 3, found)
            })?
            .as_pixels();
        let indices = if has_indices {
            let expected_len = parsing
                .get_u24_be()
                .ok_or_else(|| ParseError::missing_content(&parsing))?;
            let bzz = parsing.take().bzz();
            Some(EncodedIndices { expected_len, bzz })
        } else {
            None
        };
        if !parsing.is_empty() {
            return Err(ParseError::excess(&parsing));
        }
        Ok(Palette {
            version,
            entries,
            indices,
        })
    }
}

/// A single index parsed from an [`Fgbz`] element.
///
/// You can’t do much with a value of this type, except convert it into a `u16` with
/// [`PaletteIndex::into`].
#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct PaletteIndex([u8; 2]);

impl PaletteIndex {
    fn parse_from(parsing: Parsing<'_>, expected_len: u32) -> Result<&[Self], ParseError> {
        match parsing.len().cmp(&(expected_len as usize * 2)) {
            Ordering::Less => Err(ParseError::content_len_mismatch(
                &parsing,
                expected_len as usize * 2,
                parsing.len(),
            )),
            Ordering::Equal => Ok(indices_from_bytes(parsing.inner)),
            Ordering::Greater => Err(ParseError::excess(&parsing)),
        }
    }
}

impl Into<u16> for PaletteIndex {
    fn into(self) -> u16 {
        u16::from_be_bytes(self.0)
    }
}

fn indices_from_bytes(bytes: &[u8]) -> &[PaletteIndex] {
    assert!(bytes.len() % 2 == 0);
    let ptr = bytes.as_ptr().cast();
    let len = bytes.len() / 2;
    // SAFETY PaletteIndex is repr(transparent), bytes.len() is even, and [u8; 2] has the same
    // alignment as u8
    unsafe { std::slice::from_raw_parts(ptr, len) }
}

/// An element that contains an IW44-encoded image layer.
///
/// This might represent an `FG44` chunk, a `BG44` chunk, or a `TH44` chunk.
#[derive(Clone, Debug)]
pub struct Iw44<'raw>(pub(crate) Leaf<'raw>);

impl<'raw> Iw44<'raw> {
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }

    pub fn parse(&self) -> Result<ParsedIw44<'raw>, ParseError> {
        ParsedIw44::parse_from(self.0.content.clone())
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Iw44Version {
    major: u8,
    minor: u8,
}

impl Iw44Version {
    pub fn new(major: u8, minor: u8) -> Option<Self> {
        if major & (1 << 7) == 0 {
            Some(Self { major, minor })
        } else {
            None
        }
    }

    pub fn major(self) -> u8 {
        self.major
    }

    pub fn minor(self) -> u8 {
        self.minor
    }
}

impl std::fmt::Display for Iw44Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Iw44ColorsKind {
    YCbCr,
    Grayscale,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Iw44Kind {
    Head {
        version: Iw44Version,
        colors: Iw44ColorsKind,
        width: u16,
        height: u16,
        initial_cdc: u8,
    },
    Tail(NonZeroU8),
}

#[derive(Clone, Debug)]
pub struct ParsedIw44<'raw> {
    pub kind: Iw44Kind,
    pub num_slices: u8,
    bytes: &'raw [u8],
}

impl<'raw> ParsedIw44<'raw> {
    fn parse_from(mut parsing: Parsing<'raw>) -> Result<Self, ParseError> {
        let serial = parsing.get_u8_or_missing_content()?;
        let num_slices = parsing.get_u8_or_missing_content()?;
        if let Some(n) = NonZeroU8::new(serial) {
            Ok(ParsedIw44 {
                kind: Iw44Kind::Tail(n),
                num_slices,
                bytes: parsing.inner,
            })
        } else {
            let byte = parsing.get_u8_or_missing_content()?;
            let colors = if byte & (1 << 7) == 0 {
                Iw44ColorsKind::YCbCr
            } else {
                Iw44ColorsKind::Grayscale
            };
            let major = byte & !(1 << 7);
            let minor = parsing.get_u8_or_missing_content()?;
            let width = parsing.get_u16_be_or_missing_content()?;
            let height = parsing.get_u16_be_or_missing_content()?;
            let initial_cdc = parsing.get_u8_or_missing_content()? & !(1 << 7);
            Ok(ParsedIw44 {
                kind: Iw44Kind::Head {
                    version: Iw44Version { major, minor },
                    colors,
                    width,
                    height,
                    initial_cdc,
                },
                num_slices,
                bytes: parsing.inner,
            })
        }
    }
}

/// An `Smmr` element.
#[derive(Clone, Debug)]
pub struct Smmr<'raw>(Leaf<'raw>);

impl<'raw> Smmr<'raw> {
    /// Returns the raw bytes contained in this chunk.
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }
}

/// An element that contains a JPEG2000-encoded image.
///
/// This might represent an `FGjp` chunk or a `BGjp` chunk.
#[derive(Clone, Debug)]
pub struct Jpeg<'raw>(Leaf<'raw>);

impl<'raw> Jpeg<'raw> {
    /// Returns the raw bytes contained in this chunk.
    pub fn bytes(&self) -> &'raw [u8] {
        self.0.content_bytes()
    }
}

/// A fallible iterator that parses successive elements from a [`Page`].
///
/// Create a value of this type by calling [`Page::elements`].
#[derive(Clone, Debug)]
pub struct PageElements<'raw> {
    pub(crate) chunks: LeafChunks<'raw>,
}

impl<'raw> PageElements<'raw> {
    /// Attempts to parse the next element in this page.
    ///
    /// Returns `Ok(None)` after the last element has been yielded, or an `Err` value if parsing
    /// fails.
    pub fn try_next(&mut self) -> Result<Option<Element<'raw>>, ParseError> {
        self.chunks.try_next().map(|option| {
            option.map(|chunk| match &chunk.id {
                b"ANTa" => Element::Ant(Ant::from_anta(chunk)),
                b"ANTz" => Element::Ant(Ant::from_antz(chunk)),
                b"TXTa" => Element::Txt(Txt::from_txta(chunk)),
                b"TXTz" => Element::Txt(Txt::from_txtz(chunk)),
                b"Djbz" => Element::Djbz(Djbz(chunk)),
                b"Sjbz" => Element::Sjbz(Sjbz(chunk)),
                b"FG44" => Element::Fg44(Iw44(chunk)),
                b"BG44" => Element::Bg44(Iw44(chunk)),
                b"FGbz" => Element::Fgbz(Fgbz(chunk)),
                b"INCL" => Element::Incl(Incl(chunk)),
                b"BGjp" => Element::Bgjp(Jpeg(chunk)),
                b"FGjp" => Element::Fgjp(Jpeg(chunk)),
                b"Smmr" => Element::Smmr(Smmr(chunk)),
                // TODO intelligently account for chunks that existed in previous
                // versions of the standard?
                _ => Element::Unrecognized(chunk),
            })
        })
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'raw> fallible_iterator::FallibleIterator for PageElements<'raw> {
    type Item = Element<'raw>;
    type Error = ParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}

/// A DjVu page.
#[derive(Clone, Debug)]
pub struct Page<'raw> {
    pub width: u16,
    pub height: u16,
    pub version: PageVersion,
    pub dpi: u16,
    pub gamma: u8,
    pub rotation: Rotation,
    chunks: LeafChunks<'raw>,
}

impl<'raw> Page<'raw> {
    pub(crate) fn parse_from(mut chunks: LeafChunks<'raw>) -> Result<Self, ParseError> {
        let first = chunks
            .try_next()?
            .ok_or_else(|| ParseError::missing_chunk(&chunks.parsing, *b"INFO"))?;
        if &first.id != b"INFO" {
            return Err(ParseError::unexpected_chunk(&chunks.parsing, first.id));
        }
        let mut info = first.content;
        let width = info.get_u16_be_or_missing_content()?;
        let height = info.get_u16_be_or_missing_content()?;
        let minor = info.get_u8_or_missing_content()?;
        let major = info.get_u8_or_missing_content()?;
        let dpi = info.get_u16_le_or_missing_content()?;
        let gamma = info.get_u8_or_missing_content()?;
        let flags = info.get_u8_or_missing_content()?;
        let rotation = match flags & 0b111 {
            1 => Rotation::Up,
            6 => Rotation::Ccw,
            2 => Rotation::Down,
            5 => Rotation::Cw,
            _ => Rotation::default(),
        };
        Ok(Self {
            width,
            height,
            version: PageVersion { major, minor },
            dpi,
            gamma,
            rotation,
            chunks,
        })
    }

    /// Returns a fallible iterator that will parse the [`Element`]s in this page.
    pub fn elements(&self) -> PageElements<'raw> {
        PageElements {
            chunks: self.chunks.clone(),
        }
    }
}
