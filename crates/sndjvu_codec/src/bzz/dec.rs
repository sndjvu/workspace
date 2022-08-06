//! BZZ decoding.

use crate::Step::{self, *};
use crate::zp;
use super::{Speed, Symbol, Mtf, Scratch, NUM_CONTEXTS};
use alloc::boxed::Box;
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

impl From<core::convert::Infallible> for Error {
    fn from(x: core::convert::Infallible) -> Self {
        match x {}
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
pub(super) fn bwt_inv(marker: u32, slice: &mut [u8], scratch: &mut Scratch) {
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

pub struct Start<'dec> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    zp: zp::Decoder<'dec>,
    // reuse the allocation, since it has to be boxed anyway
    mtf_array: Box<[Symbol; 256]>,
}

pub fn start(bzz: &[u8]) -> Start<'_> {
    Start {
        contexts: Box::new([zp::Context::NEW; NUM_CONTEXTS]),
        zp: zp::Decoder::new(bzz),
        mtf_array: Box::new([Symbol(0); 256]),
    }
}

impl<'dec> Start<'dec> {
    pub fn step<'scratch>(self, scratch: &'scratch mut Scratch) -> Step<Option<Block<'dec, 'scratch>>, StartSave> {
        let mut zp = match self.zp.provision(24 + 2) {
            Complete(dec) => dec,
            Incomplete(zp) => {
                return Incomplete(StartSave {
                    contexts: self.contexts,
                    mtf_array: self.mtf_array,
                    zp
                });
            }
        };

        let block_size = decode_u24(&mut zp);
        if block_size == 0 {
            return Complete(None);
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
        let mtf = Mtf::new(speed, self.mtf_array);

        scratch.shadow.clear();
        Complete(Some(Block {
            contexts: self.contexts,
            zp,
            progress: BlockProgress {
                size: block_size,
                i: 0,
                marker: None,
                mtf,
                mtf_index: Some(3),
            },
            scratch,
        }))
    }
}

pub struct StartSave {
    zp: zp::dec::DecoderSave,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    mtf_array: Box<[Symbol; 256]>,
}

impl StartSave {
    pub fn resume(self, bzz: &[u8]) -> Start<'_> {
        Start {
            zp: self.zp.resume(bzz),
            contexts: self.contexts,
            mtf_array: self.mtf_array,
        }
    }

    pub fn seal<'dec>(self) -> Start<'dec> {
        Start {
            zp: self.zp.seal(),
            contexts: self.contexts,
            mtf_array: self.mtf_array,
        }
    }
}

// state that exists only during the decoding of a block
struct BlockProgress {
    size: u32,
    i: u32,
    marker: Option<u32>,
    mtf: Mtf,
    mtf_index: Option<u8>,
}

pub struct Block<'dec, 'scratch> {
    zp: zp::Decoder<'dec>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress,
    scratch: &'scratch mut Scratch,
}

pub struct BlockSave<'scratch> {
    zp: zp::dec::DecoderSave,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress,
    scratch: &'scratch mut Scratch,
}

pub struct Shuffle<'scratch> {
    marker: u32,
    scratch: &'scratch mut Scratch,
}

impl<'scratch> Shuffle<'scratch> {
    pub fn len(&self) -> usize {
        self.scratch.shadow.len() - 1
    }

    pub fn run(self, out: &mut [u8]) {
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

impl<'scratch> BlockSave<'scratch> {
    pub fn resume<'dec>(self, bzz: &'dec [u8]) -> Block<'dec, 'scratch> {
        Block { zp: self.zp.resume(bzz), contexts: self.contexts, progress: self.progress, scratch: self.scratch }
    }

    pub fn seal<'dec>(self) -> Block<'dec, 'scratch> {
        Block { zp: self.zp.seal(), contexts: self.contexts, progress: self.progress, scratch: self.scratch }
    }
}

impl<'dec, 'scratch> Block<'dec, 'scratch> {
    pub fn step(self) -> Result<Step<(Shuffle<'scratch>, Start<'dec>), BlockSave<'scratch>>, Error> {
        let Self { mut contexts, mut zp, mut progress, scratch } = self;
        while progress.i < progress.size {
            zp = match zp.provision(16) {
                Complete(dec) => dec,
                Incomplete(zp) => {
                    return Ok(Incomplete(BlockSave {
                        contexts,
                        progress,
                        zp,
                        scratch,
                    }))
                }
            };

            let mtf_index = progress.mtf_index.map_or(256, usize::from);
            let start = mtf_index.min(2);
            let next = if zp.decode(&mut contexts[start]) {
                0
            } else if zp.decode(&mut contexts[start + 3]) {
                1
            } else if let Some(x) = (1..8).find(|&s| zp.decode(&mut contexts[4 + (1 << s)])) {
                (1 << x) + decode_u8(&mut zp, 5 + (1 << x), x, &mut contexts)
            } else {
                progress.mtf_index = None;
                if let Some(old) = progress.marker.replace(progress.i) {
                    return Err(Error {
                        kind: ErrorKind::ExtraMarker {
                            first: old,
                            second: progress.i,
                        },
                    });
                }
                scratch.shadow.push(0);
                progress.i += 1;
                continue;
            };

            progress.mtf_index = Some(next);
            let symbol = progress.mtf.do_rotation(next);
            scratch.shadow.push(symbol.get());
            progress.i += 1;
        }

        // inverse Burrows-Wheeler step
        let marker = progress.marker.ok_or(Error { kind: ErrorKind::MissingMarker })?;

        // ready to decode the next block header
        Ok(Complete((
            Shuffle { marker, scratch },
            Start {
                contexts,
                zp,
                mtf_array: progress.mtf.into_inner(),
            },
        )))
    }
}
