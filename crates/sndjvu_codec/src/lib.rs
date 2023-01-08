#![no_std]
#![deny(
    elided_lifetimes_in_paths,
    unsafe_op_in_unsafe_fn,
    unused_must_use,

    clippy::pattern_type_mismatch,
)]
#![cfg_attr(sndjvu_doc_cfg, feature(doc_cfg, doc_auto_cfg))]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

/// Represents the outcome of a coding operation, if no error was encountered.
pub enum Step<L, R> {
    /// Enough bytes were available and the coding operation completed successfully.
    Complete(L),
    /// Not enough input bytes were provided to complete the coding operation.
    Incomplete(R),
}

pub(crate) mod zp;
#[cfg(feature = "bzz")]
pub mod bzz;
