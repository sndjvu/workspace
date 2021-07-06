// sndjvu
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

pub use sndjvu_codec as codec;
pub use sndjvu_format as format;

#[derive(Clone, Debug)]
pub struct MissingComponentError {
    pub expected: format::multi_page::ComponentKind,
}

impl std::fmt::Display for MissingComponentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`{:?}` component listed in document metadata is missing",
            self.expected
        )
    }
}

impl std::error::Error for MissingComponentError {}

#[derive(Clone, Debug)]
pub struct ComponentMetaMismatchError {
    pub expected: format::multi_page::ComponentKind,
    pub found: format::multi_page::ComponentKind,
}

impl std::fmt::Display for ComponentMetaMismatchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "document metadata lists a `{:?}` component, but a `{:?}` component was found",
            self.expected, self.found
        )
    }
}

impl std::error::Error for ComponentMetaMismatchError {}
