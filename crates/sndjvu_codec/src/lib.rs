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

/// `Result`-like type used to convey the result of a coding operation.
pub enum Update<T, U> {
    /// Indicates that the coding operation succeeded.
    Success(T),
    /// Indicates that the coding operation couldn't be completed because the coder ran out of
    /// input.
    Suspend(U),
}

pub(crate) mod zp;
#[cfg(feature = "bzz")]
pub mod bzz;
