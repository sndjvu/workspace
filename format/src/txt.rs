// sndjvu_format::txt
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

use crate::chunk::Leaf;
use crate::{Bzz, BzzBuffer, ParseError, Parsing};
use bstr::{BStr, ByteSlice};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LayoutVersion(pub u8);

impl std::fmt::Display for LayoutVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u8> for LayoutVersion {
    fn from(x: u8) -> Self {
        Self(x)
    }
}

impl Into<u8> for LayoutVersion {
    fn into(self) -> u8 {
        self.0
    }
}

impl LayoutVersion {
    pub const CURRENT: Self = Self(1);
}

#[derive(Clone, Debug)]
pub struct RawTxt<'raw> {
    parsing: Parsing<'raw>,
}

impl<'raw> RawTxt<'raw> {
    pub fn bytes(&self) -> &'raw [u8] {
        self.parsing.inner
    }

    pub fn parse(&self) -> Result<Layout<'raw>, ParseError> {
        Layout::parse_from(self.parsing.clone())
    }
}

#[derive(Clone, Debug)]
pub struct EncodedTxt<'raw> {
    bzz: Bzz<'raw>,
}

impl<'raw> EncodedTxt<'raw> {
    pub fn bytes(&self) -> &'raw [u8] {
        self.bzz.raw
    }

    pub fn decode_and_parse<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Result<Layout<'dec>, ParseError> {
        let parsing = self.bzz.parsing_decoded(buffer, scratch);
        Layout::parse_from(parsing)
    }
}

#[derive(Clone, Debug)]
pub enum Txt<'raw> {
    Raw(RawTxt<'raw>),
    Encoded(EncodedTxt<'raw>),
}

impl<'raw> Txt<'raw> {
    pub(crate) fn from_txta(chunk: Leaf<'raw>) -> Self {
        Self::Raw(RawTxt {
            parsing: chunk.content,
        })
    }

    pub(crate) fn from_txtz(chunk: Leaf<'raw>) -> Self {
        Self::Encoded(EncodedTxt {
            bzz: chunk.content.bzz(),
        })
    }
}

pub struct LayoutZones<'a> {
    parsing: Parsing<'a>,
}

impl<'a> LayoutZones<'a> {
    pub fn try_next(&mut self) -> Result<Option<Zone>, ParseError> {
        let kind = match self.parsing.get_u8() {
            None => return Ok(None),
            Some(1) => ZoneKind::Page,
            Some(2) => ZoneKind::Column,
            Some(3) => ZoneKind::Region,
            Some(4) => ZoneKind::Paragraph,
            Some(5) => ZoneKind::Line,
            Some(6) => ZoneKind::Word,
            Some(7) => ZoneKind::Character,
            Some(other) => return Err(ParseError::unexpected_val(&self.parsing, other)),
        };
        let offset_x = self.parsing.get_u16_be_or_missing_content()?;
        let offset_x = cvt_coord(offset_x);
        let offset_y = self.parsing.get_u16_be_or_missing_content()?;
        let offset_y = cvt_coord(offset_y);
        let width = self.parsing.get_u16_be_or_missing_content()?;
        let width = cvt_coord(width);
        let height = self.parsing.get_u16_be_or_missing_content()?;
        let height = cvt_coord(height);
        let _ = self.parsing.get_u16_be_or_missing_content()?;
        let text_len = self.parsing.get_u24_be_or_missing_content()?;
        let num_children = self.parsing.get_u24_be_or_missing_content()?;
        Ok(Some(Zone {
            kind,
            offset_x,
            offset_y,
            width,
            height,
            text_len,
            num_children,
        }))
    }
}

pub struct Layout<'a> {
    pub text: &'a BStr,
    pub version: LayoutVersion,
    rest: Parsing<'a>,
}

impl<'a> Layout<'a> {
    fn parse_from(mut parsing: Parsing<'a>) -> Result<Self, ParseError> {
        let text_len = parsing.get_u24_be_or_missing_content()?;
        let text = parsing
            .get_span(text_len as usize)
            .map_err(|found| ParseError::content_len_mismatch(&parsing, text_len as usize, found))?
            .as_bstr();
        let version = parsing.get_u8_or_missing_content()?.into();
        Ok(Layout {
            text,
            version,
            rest: parsing,
        })
    }

    pub fn zones(&self) -> LayoutZones<'a> {
        LayoutZones {
            parsing: self.rest.clone(),
        }
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'a> fallible_iterator::FallibleIterator for LayoutZones<'a> {
    type Item = Zone;
    type Error = ParseError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Zone {
    pub kind: ZoneKind,
    pub offset_x: i16,
    pub offset_y: i16,
    pub width: i16,
    pub height: i16,
    pub text_len: u32,
    pub num_children: u32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ZoneKind {
    Page = 1,
    Column,
    Region,
    Paragraph,
    Line,
    Word,
    Character,
}

fn cvt_coord(x: u16) -> i16 {
    (x ^ 0x80_00) as _
}
