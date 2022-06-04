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

/// Wrapper type used to indicate how much of an output buffer was written by an encoding
/// operation.
pub struct Progress<'a, U = ()> {
    /// Prefix of the output slice that was written by the encoder.
    pub written: &'a mut [u8],
    /// Suffix of the output slice that was not written by the encoder.
    pub unwritten: &'a mut [u8],
    /// Payload, generally used to hold an `EncoderSave` value that allows resuming encoding.
    pub save: U,
}

pub(crate) mod zp;
