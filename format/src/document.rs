// sndjvu_format::document
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

use crate::chunk::FormChunks;
use crate::multi_page::MultiPage;
use crate::page::Page;
use crate::{ParseError, Parsing};

pub const DJVU_MAGIC: [u8; 4] = [b'A', b'T', b'&', b'T'];

#[derive(Clone, Debug)]
pub enum Document<'raw> {
    SinglePage(Page<'raw>),
    MultiPage(MultiPage<'raw>),
}

impl<'raw> Document<'raw> {
    fn parse_from(mut parsing: Parsing<'raw>) -> Result<Self, ParseError> {
        if parsing.get_bytes::<4>() != Some(DJVU_MAGIC) {
            return Err(ParseError::missing_magic(&parsing));
        }
        let mut forms = FormChunks { parsing };
        let outer = match forms.try_next() {
            Ok(None) => return Err(ParseError::empty_document(&forms.parsing)),
            Ok(Some(form)) => form,
            Err(e) => return Err(e),
        };
        match &outer.kind {
            b"DJVU" => {
                let page = Page::parse_from(outer.leaves())?;
                Ok(Document::SinglePage(page))
            }
            b"DJVM" => {
                let multi_page = MultiPage::parse_from(outer.leaves())?;
                Ok(Document::MultiPage(multi_page))
            }
            _ => Err(ParseError::unexpected_form(&forms.parsing, outer.kind)),
        }
    }
}

pub fn parse_document(raw: &[u8]) -> Result<Document<'_>, ParseError> {
    Document::parse_from(Parsing::new(raw))
}
