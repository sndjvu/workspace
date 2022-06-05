#![no_std]
#![deny(
    elided_lifetimes_in_paths,
    unsafe_op_in_unsafe_fn,
    unused_must_use,

    clippy::pattern_type_mismatch,
)]
#![cfg_attr(sndjvu_backtrace, feature = "backtrace")]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

use core::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug)]
pub struct TxtVersion(pub u8);

impl TxtVersion {
    pub const CURRENT: Self = Self(1);
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

    fn pack(self, is_bundled: IsBundled) -> [u8; 1] {
        [self.0 | ((is_bundled as u8) << 7)]
    }
}

impl Display for DirmVersion {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
enum IsBundled {
    #[allow(unused)]
    No,
    Yes,
}

#[derive(Clone, Copy, Debug)]
pub enum ComponentKind {
    Djvi,
    Djvu,
    Thum,
}

impl ComponentKind {
    fn name(&self) -> &'static [u8; 4] {
        match self {
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

pub mod annot;
pub mod parsing;
pub mod ser;
pub(crate) mod shim;
