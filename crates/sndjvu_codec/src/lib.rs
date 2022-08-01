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

pub enum Step<L, R> {
    Complete(L),
    Incomplete(R),
}

pub(crate) mod zp;
#[cfg(feature = "bzz")]
pub mod bzz;
