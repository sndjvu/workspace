#![no_std]
#![deny(
    elided_lifetimes_in_paths,
    unsafe_op_in_unsafe_fn,
    unused_must_use,

    clippy::pattern_type_mismatch,
)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub struct TxtVersion(pub u8);

impl TxtVersion {
    pub const CURRENT: Self = Self(1);
}

pub struct Iw44Version {
    _major: u8,
    _minor: u8,
}

impl Iw44Version {
    pub const CURRENT: Self = Self { _major: 1, _minor: 2 };
}

pub struct FgbzVersion(u8);

impl FgbzVersion {
    pub const CURRENT: Self = Self(0);
}

pub struct PageVersion {
    pub major: u8,
    pub minor: u8,
}

impl PageVersion {
    pub const CURRENT: Self = Self { major: 0, minor: 26 };
}

pub enum PageRotation {
    Up = 1,
    Ccw = 6,
    Down = 2,
    Cw = 5,
}

pub struct MultiPageVersion(u8);

impl MultiPageVersion {
    pub const CURRENT: Self = Self(1);
}

pub enum ComponentKind {
    Djvi,
    Djvu,
    Thum,
}

pub enum ZoneKind {
    Page = 1,
    Column,
    Region,
    Paragraph,
    Line,
    Word,
    Character,
}

pub enum Iw44ColorSpace {
    YCbCr,
    Gray,
}
