// sndjvu_format::chunk
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

use crate::{ParseError, Parsing};
#[allow(unused)]
use std::convert::{TryFrom, TryInto};

/// A chunk of data extracted from the IFF85 structure of a DjVu document.
#[derive(Clone, Debug)]
pub struct Leaf<'raw> {
    /// A four-byte identifier describing the content of this chunk.
    pub id: [u8; 4],
    pub(crate) content: Parsing<'raw>,
}

impl<'raw> Leaf<'raw> {
    /// Returns the raw bytes contained in this chunk.
    pub fn content_bytes(&self) -> &'raw [u8] {
        self.content.inner
    }
}

#[derive(Clone, Debug)]
pub struct LeafChunks<'raw> {
    pub(crate) parsing: Parsing<'raw>,
}

impl<'raw> LeafChunks<'raw> {
    pub(crate) fn try_next(&mut self) -> Result<Option<Leaf<'raw>>, ParseError> {
        if self.parsing.is_empty() {
            return Ok(None);
        }

        let saved = self.parsing.clone();
        let result = (|| {
            let id = self
                .parsing
                .get_bytes::<4>()
                .ok_or_else(|| ParseError::incomplete_chunk(&self.parsing))?;
            if !id.is_ascii() {
                return Err(ParseError::junk(&self.parsing));
            }
            if &id == b"FORM" {
                return Err(ParseError::unexpected_chunk(&self.parsing, id));
            }
            let len = self
                .parsing
                .get_u32_be()
                .ok_or_else(|| ParseError::incomplete_chunk(&self.parsing))?;
            let content = self
                .parsing
                .split_off(len as usize)
                .map_err(|found| {
                    ParseError::chunk_len_mismatch(&self.parsing, len as usize, found)
                })?
                .leaf(id);
            Ok(Leaf { id, content })
        })();
        match result {
            Ok(leaf) => {
                self.parsing.advance_to_even();
                Ok(Some(leaf))
            }
            Err(e) => {
                self.parsing = saved;
                Err(e)
            }
        }
    }

    pub(crate) fn peek_id(&self) -> Option<[u8; 4]> {
        let id = self.parsing.inner.get(..4)?;
        // if `get` returned a `Some` then its length is exactly 4
        Some(id.try_into().unwrap())
    }

    pub(crate) fn into_forms(self) -> FormChunks<'raw> {
        FormChunks {
            parsing: self.parsing,
        }
    }
}

/// A chunk that contains other chunks.
#[derive(Clone, Debug)]
pub struct Form<'raw> {
    /// The first four content bytes of this chunk, which identify its kind.
    pub(crate) kind: [u8; 4],
    pub(crate) content: Parsing<'raw>,
}

impl<'raw> Form<'raw> {
    pub(crate) fn leaves(self) -> LeafChunks<'raw> {
        LeafChunks {
            parsing: self.content,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FormChunks<'raw> {
    pub(crate) parsing: Parsing<'raw>,
}

impl<'raw> FormChunks<'raw> {
    pub(crate) fn try_next(&mut self) -> Result<Option<Form<'raw>>, ParseError> {
        if self.parsing.is_empty() {
            return Ok(None);
        }

        let saved = self.parsing.clone();
        let result = (|| {
            let id = self
                .parsing
                .get_bytes::<4>()
                .ok_or_else(|| ParseError::incomplete_chunk(&self.parsing))?;
            if !id.is_ascii() {
                return Err(ParseError::junk(&self.parsing));
            }
            if &id != b"FORM" {
                return Err(ParseError::unexpected_chunk(&self.parsing, id));
            }
            let len = self
                .parsing
                .get_u32_be()
                .ok_or_else(|| ParseError::incomplete_chunk(&self.parsing))?;
            let mut parsing = self.parsing.split_off(len as usize).map_err(|found| {
                ParseError::chunk_len_mismatch(&self.parsing, len as usize, found)
            })?;
            let kind = parsing
                .get_bytes::<4>()
                .ok_or_else(|| ParseError::incomplete_form(&self.parsing))?;
            let content = parsing.form(kind);
            Ok(Form { kind, content })
        })();
        match result {
            Ok(form) => {
                self.parsing.advance_to_even();
                Ok(Some(form))
            }
            Err(e) => {
                self.parsing = saved;
                Err(e)
            }
        }
    }
}
