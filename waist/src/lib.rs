// sndjvu_waist
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

/*! Defines a minimal interface for connecting different SnDjVu crates.

This crate is an implementation detail, and all its items are re-exported elsewhere:

- [`BzzBuffer`] is re-exported by `sndjvu_format`
*/

pub trait BzzBuffer {
    type Scratch;
    fn decode_bytes(&mut self, bytes: &[u8], scratch: &mut Self::Scratch) -> &[u8];
}
