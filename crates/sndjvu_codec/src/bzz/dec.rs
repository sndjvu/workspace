//! BZZ decoding.
//!
//! This module provides both high-level and low-level interfaces to BZZ
//! decoding. The high-level interface consists of [`decompress`] and [`decompress_oneshot`].
//! The low-level interface consists of the following types and their associated functions:
//!
//! - [`Decoder`]
//! - [`DecoderSave`]
//! - [`DecodeBlock`]
//! - [`DecodeBlockSave`]
//! - [`ShuffleBlock`]
//!
//! The following restrictions apply to the high-level interface:
//!
//! - the input consists of a single `&[u8]` 
//! - the output is concatenated into a single `Vec<u8>`
//! - decompression is entirely sequential (no concurrency)
//!
//! These restrictions are lifted if you use the less convenient low-level interface.

use crate::Update;
use super::{Speed, Symbol, Mtf, Scratch, NUM_CONTEXTS};
use crate::zp;
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::fmt::{Display, Formatter};
use core::mem::replace;

/// An error encountered while decoding a BZZ block.
///
/// Encountering such an error means that the data passed to the decoder was not valid BZZ.
/// There is no good way for the decoder to recover in this situation, and callers who
/// encounter such an error should give up on trying to decode the data.
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

#[derive(Debug)]
enum ErrorKind {
    MissingMarker,
    ExtraMarker {
        first: u32,
        second: u32,
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self.kind {
            ErrorKind::MissingMarker => write!(f, "marker position for block was not encoded")?,
            ErrorKind::ExtraMarker { first, second } => write!(f, "marker position for block was encoded more than once ({first}, then {second})")?,
        }
        Ok(())
    }
}

#[cfg(feature = "std")]
impl std::error::Error for Error {}

fn postincrement(n: &mut u32) -> u32 {
    let old = *n;
    *n += 1;
    old
}

// inverts the Burrows-Wheeler transform, using the same algorithm as DjVuLibre
fn bwt_inv(marker: u32, slice: &mut [u8], scratch: &mut Scratch) {
    let Scratch {
        // like djvulibre's posc
        ref mut shadow,
        // as in djvulibre
        ref mut counts,
        // like djvulibre's posn
        ref mut ranks,
    } = scratch;

    assert_eq!(slice.len() + 1, shadow.len());
    ranks.clear();
    **counts = [0; 256];
    ranks.extend(shadow.iter().zip(0u32..).map(|(&sym, i)| {
        if i == marker {
            0
        } else {
            postincrement(&mut counts[sym as usize])
        }
    }));

    let total = counts.iter_mut().fold(1, |acc, k| acc + replace(k, acc));
    assert_eq!(total as usize, shadow.len());

    let pos = slice.iter_mut().rev().fold(0, |acc, dest| {
        let sym = shadow[acc as usize];
        *dest = sym;
        counts[sym as usize] + ranks[acc as usize]
    });
    assert_eq!(pos, marker);
}

// used to decode block size
fn decode_u24(zp: &mut zp::Decoder<'_>) -> u32 {
    let mut n = 1;
    while n < 1 << 24 {
        n = (n << 1) | (zp.decode_passthrough() as u32);
    }
    n - (1 << 24)
}

// used to decode MTF indices
fn decode_u8(
    zp: &mut zp::Decoder<'_>,
    start: u8,
    num_bits: u32,
    contexts: &mut [zp::Context; NUM_CONTEXTS],
) -> u8 {
    let mut n = 1;
    while n < 1 << num_bits {
        n = (n << 1) | (zp.decode(&mut contexts[start as usize + n as usize - 1]) as u8);
    }
    n - (1 << num_bits)
}

pub struct Decoder<'a> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    zp: zp::Decoder<'a>,
    // reuse the allocation, since it has to be boxed anyway
    mtf_array: Box<[Symbol; 256]>,
}

impl<'a> Decoder<'a> {
    pub fn new(bzz: &'a [u8]) -> Self {
        const NEW_CONTEXT: zp::Context = zp::Context::new();
        Self {
            contexts: Box::new([NEW_CONTEXT; NUM_CONTEXTS]),
            zp: zp::Decoder::new(bzz),
            mtf_array: Box::new([Symbol(0); 256]),
        }
    }

    pub fn block<'b>(self, scratch: &'b mut Scratch) -> Update<Option<DecodeBlock<'a, 'b>>, DecoderSave> {
        let mut zp = match self.zp.provision(24 + 2) {
            Update::Success(dec) => dec,
            Update::Suspend(zp) => {
                return Update::Suspend(DecoderSave {
                    contexts: self.contexts,
                    mtf_array: self.mtf_array,
                    zp
                });
            }
        };

        let block_size = decode_u24(&mut zp);
        #[cfg(sndjvu_debug_bzz)] std::eprintln!("BLOCK {block_size}");
        if block_size == 0 {
            return Update::Success(None);
        }

        let speed = if zp.decode_passthrough() {
            if zp.decode_passthrough() {
                Speed::Two
            } else {
                Speed::One
            }
        } else {
            Speed::Zero
        };
        #[cfg(sndjvu_debug_bzz)] std::eprintln!("SPEED {}",
            match speed {
                Speed::Zero => 0,
                Speed::One => 1,
                Speed::Two => 2,
            }
        );
        let mtf = Mtf::new(speed, self.mtf_array);

        scratch.shadow.clear();
        Update::Success(Some(DecodeBlock {
            contexts: self.contexts,
            zp,
            progress: BlockProgress {
                size: block_size,
                i: 0,
                marker: None,
                mtf,
                mtf_index: Some(3),
                scratch,
            },
        }))
    }
}

pub struct DecoderSave {
    zp: zp::dec::DecoderSave,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    mtf_array: Box<[Symbol; 256]>,
}

impl DecoderSave {
    pub fn resume(self, bzz: &[u8]) -> Decoder<'_> {
        Decoder {
            zp: self.zp.resume(bzz),
            contexts: self.contexts,
            mtf_array: self.mtf_array,
        }
    }

    pub fn seal<'a>(self) -> Decoder<'a> {
        Decoder {
            zp: self.zp.seal(),
            contexts: self.contexts,
            mtf_array: self.mtf_array,
        }
    }
}

// state that exists only during the decoding of a block
struct BlockProgress<'b> {
    size: u32,
    i: u32,
    marker: Option<u32>,
    mtf: Mtf,
    mtf_index: Option<u8>,
    // the scratch buffer is "locked" throughout the decoding of a given block
    // this prevents logic errors caused by using different scratch buffers
    // before and after a suspend/resume
    scratch: &'b mut Scratch,
}

pub struct DecodeBlock<'a, 'b> {
    zp: zp::Decoder<'a>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress<'b>,
}

pub struct DecodeBlockSave<'b> {
    zp: zp::dec::DecoderSave,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress<'b>,
}

pub struct ShuffleBlock<'b> {
    marker: u32,
    scratch: &'b mut Scratch,
}

impl<'b> ShuffleBlock<'b> {
    pub fn len(&self) -> usize {
        self.scratch.shadow.len() - 1
    }

    pub fn shuffle(self, out: &mut [u8]) {
        if out.len() != self.len() {
            panic!(
                "usage error: passed a slice of the wrong length to `ShuffleBlock::shuffle` \
                 (expected {}, found {})",
                self.len(),
                out.len(),
            )
        }
        bwt_inv(self.marker, out, self.scratch);
    }
}

impl<'b> DecodeBlockSave<'b> {
    pub fn resume<'a>(self, bzz: &'a [u8]) -> DecodeBlock<'a, 'b> {
        DecodeBlock { zp: self.zp.resume(bzz), contexts: self.contexts, progress: self.progress }
    }

    pub fn seal<'a>(self) -> DecodeBlock<'a, 'b> {
        DecodeBlock { zp: self.zp.seal(), contexts: self.contexts, progress: self.progress }
    }
}

impl<'a, 'b> DecodeBlock<'a, 'b> {
    pub fn decode(self) -> Result<Update<(ShuffleBlock<'b>, Decoder<'a>), DecodeBlockSave<'b>>, Error>
    {
        let Self { mut contexts, mut zp, mut progress } = self;
        while progress.i < progress.size {
            zp = match zp.provision(16) {
                Update::Success(dec) => dec,
                Update::Suspend(zp) => {
                    return Ok(Update::Suspend(DecodeBlockSave {
                        contexts,
                        progress,
                        zp,
                    }))
                }
            };

            let mtf_index = progress.mtf_index.map_or(256, usize::from);
            #[cfg(sndjvu_debug_bzz)] std::eprintln!("MTF {mtf_index}");
            let start = mtf_index.min(2);
            let next = if zp.decode(&mut contexts[start]) {
                0
            } else if zp.decode(&mut contexts[start + 3]) {
                1
            } else if let Some(x) = (1..8).find(|&s| zp.decode(&mut contexts[4 + (1 << s)])) {
                (1 << x) + decode_u8(&mut zp, 5 + (1 << x), x, &mut contexts)
            } else {
                progress.mtf_index = None;
                #[cfg(sndjvu_debug_bzz)] std::eprintln!("MARKER {}", progress.i);
                if let Some(old) = progress.marker.replace(progress.i) {
                    return Err(Error {
                        kind: ErrorKind::ExtraMarker {
                            first: old,
                            second: progress.i,
                        },
                    });
                }
                progress.scratch.shadow.push(0);
                progress.i += 1;
                continue;
            };

            progress.mtf_index = Some(next);
            let symbol = progress.mtf.do_rotation(next);
            progress.scratch.shadow.push(symbol.get());
            progress.i += 1;
        }

        // inverse Burrows-Wheeler step
        let marker = progress.marker.ok_or(Error { kind: ErrorKind::MissingMarker })?;

        // ready to decode the next block header
        Ok(Update::Success((
            ShuffleBlock { marker, scratch: progress.scratch },
            Decoder {
                contexts,
                zp,
                mtf_array: progress.mtf.into_inner(),
            },
        )))
    }
}

/// Decompress BZZ data from a byte slice into existing output and scratch buffers.
///
/// The output buffer `buf` will be [cleared](Vec::clear), and then each decoded block will be appended to it in turn.
pub fn decompress(
    bzz: &[u8],
    buf: &mut Vec<u8>,
    scratch: &mut Scratch,
) -> Result<(), Error> {
    buf.clear();
    let mut decoder = Decoder::new(bzz);
    loop {
        match decoder.block(scratch) {
            Update::Success(None) => break,
            Update::Success(Some(mut block)) => {
                loop {
                    match block.decode()? {
                        Update::Success((shuf, rest)) => {
                            let len = shuf.len();
                            let mark = buf.len();
                            buf.resize(mark + len, 0);
                            shuf.shuffle(&mut buf[mark..]);
                            decoder = rest;
                            break;
                        }
                        Update::Suspend(save) => block = save.seal(),
                    }
                }
            }
            Update::Suspend(save) => decoder = save.seal(),
        }
    }
    Ok(())
}

/// Conveniently decompress BZZ data from a byte slice to a new `Vec<u8>`.
///
/// This is more convenient than [`decompress`], but does not support reusing
/// the output buffer or scratch space.
pub fn decompress_oneshot(bzz: &[u8]) -> Result<Vec<u8>, Error> {
    let mut buf = Vec::new();
    let mut scratch = Scratch::new();
    decompress(bzz, &mut buf, &mut scratch)?;
    Ok(buf)
}

