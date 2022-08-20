#![no_std]
#![allow(
    clippy::needless_lifetimes,
    clippy::new_without_default,
    clippy::wrong_self_convention,
)]
#![deny(
    elided_lifetimes_in_paths,
    unsafe_op_in_unsafe_fn,
    unused_must_use,

    clippy::pattern_type_mismatch,
)]
#![cfg_attr(sndjvu_backtrace, feature(backtrace))]
#![cfg_attr(sndjvu_doc_cfg, feature(doc_cfg, doc_auto_cfg))]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

use core::fmt::{Debug, Display, Formatter};
use core::marker::PhantomData;

#[derive(Clone, Copy, Debug)]
pub struct TxtVersion(pub u8);

impl TxtVersion {
    pub const CURRENT: Self = Self(1);

    fn pack(self) -> [u8; 1] {
        [self.0]
    }
}

impl Display for TxtVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Iw44Version {
    major: u8,
    minor: u8,
}

impl Iw44Version {
    pub const CURRENT: Self = Self { major: 1, minor: 2 };

    fn pack(self, color_space: Iw44ColorSpace) -> [u8; 2] {
        [self.major | ((color_space as u8) << 7), self.minor]
    }
}

impl Display for Iw44Version {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FgbzVersion(u8);

impl FgbzVersion {
    pub const CURRENT: Self = Self(0);

    fn pack(self, has_indices: bool) -> [u8; 1] {
        [((has_indices as u8) << 7) | self.0]
    }
}

impl Display for FgbzVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InfoVersion {
    pub major: u8,
    pub minor: u8,
}

impl InfoVersion {
    pub const CURRENT: Self = Self { major: 0, minor: 26 };

    fn pack(self) -> [u8; 2] {
        [self.minor, self.major]
    }
}

impl Display for InfoVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum PageRotation {
    Up = 1,
    Ccw = 6,
    Down = 2,
    Cw = 5,
}

#[derive(Clone, Copy, Debug)]
pub struct DirmVersion(u8);

impl DirmVersion {
    pub const CURRENT: Self = Self(1);

    fn pack(self, is_bundled: bool) -> [u8; 1] {
        [self.0 | ((is_bundled as u8) << 7)]
    }
}

impl Display for DirmVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ComponentKind {
    Djvi,
    Djvu,
    Thum,
}

impl ComponentKind {
    fn name(&self) -> &'static [u8; 4] {
        match *self {
            Self::Djvi => b"DJVI",
            Self::Djvu => b"DJVU",
            Self::Thum => b"THUM",
        }
    }
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum Iw44ColorSpace {
    YCbCr,
    Gray,
}

#[derive(Clone, Copy, Debug)]
pub struct Cdc(u8);

impl Cdc {
    pub fn get(self) -> u8 {
        self.0
    }
}

#[derive(Clone, Copy)]
struct Bstr<B>(B);

impl<B: AsRef<[u8]>> Debug for Bstr<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        // all copied from BurntSushi's regex-automata, with thanks

        fn len_following(byte: u8) -> Option<usize> {
            if byte <= 0x7F {
                Some(1)
            } else if byte & 0b1100_0000 == 0b1000_0000 {
                None
            } else if byte <= 0b1101_1111 {
                Some(2)
            } else if byte <= 0b1110_1111 {
                Some(3)
            } else if byte <= 0b1111_0111 {
                Some(4)
            } else {
                None
            }
        }

        fn decode_char(bytes: &[u8]) -> Option<Result<char, u8>> {
            if bytes.is_empty() {
                return None;
            }
            let len = match len_following(bytes[0]) {
                None => return Some(Err(bytes[0])),
                Some(len) if len > bytes.len() => return Some(Err(bytes[0])),
                Some(1) => return Some(Ok(bytes[0] as char)),
                Some(len) => len,
            };
            match core::str::from_utf8(&bytes[..len]) {
                Ok(s) => Some(Ok(s.chars().next().unwrap())),
                Err(_) => Some(Err(bytes[0])),
            }
        }

        write!(f, "\"")?;
        let mut bytes = self.0.as_ref();
        while let Some(result) = decode_char(bytes) {
            let ch = match result {
                Ok(ch) => ch,
                Err(byte) => {
                    write!(f, r"\x{:02x}", byte)?;
                    bytes = &bytes[1..];
                    continue;
                }
            };
            bytes = &bytes[ch.len_utf8()..];
            match ch {
                '\0' => write!(f, "\\0")?,
                // ASCII control characters except \0, \n, \r, \t
                '\x01'..='\x08'
                | '\x0b'
                | '\x0c'
                | '\x0e'..='\x19'
                | '\x7f' => {
                    write!(f, "\\x{:02x}", ch as u32)?;
                }
                /* '\n' | '\r' | '\t' | */ _ => {
                    write!(f, "{}", ch.escape_debug())?;
                }
            }
        }
        write!(f, "\"")?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct PaletteIndex([u8; 2]);

impl PaletteIndex {
    fn cast_slice(arrays: &[[u8; 2]]) -> &[Self] {
        // SAFETY PaletteIndex is repr(transparent)
        unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        }
    }

    pub fn get(self) -> u16 {
        u16::from_be_bytes(self.0)
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct PaletteEntry {
    pub b: u8,
    pub g: u8,
    pub r: u8,
}

impl PaletteEntry {
    fn cast_slice(arrays: &[[u8; 3]]) -> &[Self] {
        // SAFETY PaletteEntry is repr(C) with the same layout as [u8; 3]
        unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        }
    }

    fn uncast_slice(slice: &[Self]) -> &[[u8; 3]] {
        // SAFETY PaletteEntry is repr(C) with the same layout as [u8; 3]
        unsafe {
            core::slice::from_raw_parts(
                slice.as_ptr().cast(),
                slice.len(),
            )
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct Zone {
    kind: crate::ZoneKind,
    x_offset: [u8; 2],
    y_offset: [u8; 2],
    width: [u8; 2],
    height: [u8; 2],
    _empty: [u8; 2],
    text_len: [u8; 3],
    num_children: [u8; 3],
}

fn cvt_zone_i16(bytes: [u8; 2]) -> i16 {
    i16::from_be_bytes(bytes) ^ (1 << 15)
}

impl Zone {
    fn as_bytes(&self) -> &[u8; 17] {
        unsafe { &*((self as *const Self).cast()) }
    }

    fn cast_slice(arrays: &[[u8; 17]]) -> Option<&[Self]> {
        for arr in arrays {
            let p = arr as *const _ as *const Zone;
            let x = unsafe { *core::ptr::addr_of!((*p).kind).cast::<u8>() };
            if !(1..=7).contains(&x) {
                return None;
            }
        }

        // SAFETY Zone has the same layout as [u8; 17], with no padding,
        // and we've checked that the kind fields have valid values
        let zones = unsafe {
            core::slice::from_raw_parts(
                arrays.as_ptr().cast(),
                arrays.len(),
            )
        };
        Some(zones)
    }

    fn uncast_slice(slice: &[Self]) -> &[[u8; 17]] {
        unsafe { 
            core::slice::from_raw_parts(
                slice.as_ptr().cast(),
                slice.len(),
            )
        }
    }

    pub fn kind(self) -> crate::ZoneKind {
        self.kind
    }

    pub fn x_offset(self) -> i16 {
        cvt_zone_i16(self.x_offset)
    }

    pub fn y_offset(self) -> i16 {
        cvt_zone_i16(self.y_offset)
    }

    pub fn width(self) -> i16 {
        cvt_zone_i16(self.width)
    }

    pub fn height(self) -> i16 {
        cvt_zone_i16(self.height)
    }

    pub fn text_len(self) -> u32 {
        let [b1, b2, b3] = self.text_len;
        u32::from_be_bytes([0, b1, b2, b3])
    }

    pub fn num_children(self) -> u32 {
        let [b1, b2, b3] = self.num_children;
        u32::from_be_bytes([0, b1, b2, b3])
    }
}

type PhantomMutable = PhantomData<dyn core::any::Any + Send + Sync + core::marker::Unpin>;

#[allow(non_upper_case_globals)]
const PhantomMutable: PhantomMutable = PhantomData;

pub mod annot;
pub mod parsing;
pub mod ser;
pub(crate) mod shim;
