//! BZZ encoding.

use crate::Step::{self, *};
use crate::zp;
use super::{Scratch, Speed, MtfWithInv, Symbol, NUM_CONTEXTS};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cmp::Ordering;

pub(super) fn bwt(input: &[u8], scratch: &mut Scratch) -> u32 {
    fn find_difference(left: &[u8], right: &[u8]) -> usize {
        // TODO SIMD or something
        left.iter().zip(right).take_while(|(&l, &r)| l == r).count()
    }

    let Scratch { ref mut shadow, ranks: ref mut shifts, .. } = *scratch;

    shadow.clear();
    shadow.extend_from_slice(input);
    shadow.push(0x00); // fake EOB

    shifts.clear();
    shifts.extend(0..=input.len() as u32);
    // TODO optimize this
    shifts.sort_by(|&ls, &rs| {
        let pos = find_difference(&shadow[ls as usize..], &shadow[rs as usize..]);
        let (li, ri) = (ls as usize + pos, rs as usize + pos);
        match (li == shadow.len(), ri == shadow.len()) {
            (false, false) => shadow[li].cmp(&shadow[ri]),
            (false, true) => Ordering::Greater,
            (true, false) => Ordering::Less,
            (true, true) => Ordering::Equal,
        }
    });

    shadow.clear();
    let mut marker = None;
    shadow.extend(shifts.iter().zip(0..).map(|(&shift, k)| {
        let i = shift as i32 - 1;
        let c = if i < 0 {
            marker = Some(k);
            0x00
        } else {
            input[i as usize]
        };
        c
    }));
    marker.unwrap()
}

fn encode_u24(zp: &mut zp::Encoder<'_>, mut val: u32) {
    let mut n = 1u32;
    while n < 1 << 24 {
        val = (val & 0xff_ff_ff) << 1;
        let b = val >> 24 != 0;
        zp.encode_passthrough(b);
        n = (n << 1) | b as u32;
    }
}

fn encode_u8(
    zp: &mut zp::Encoder<'_>,
    start: u8,
    num_bits: u32,
    mut val: u8,
    contexts: &mut [zp::Context; NUM_CONTEXTS],
) {
    let mut n = 1;
    while n < 1 << num_bits {
        val = (val & ((1 << num_bits) - 1)) << 1;
        let b = val >> num_bits != 0;
        zp.encode(b, &mut contexts[start as usize + n as usize - 1]);
        n = (n << 1) | b as u32;
    }
}

pub fn start(buf: &mut [u8]) -> Start<'_> {
    // check that we have enough bytes in case the caller flushes immediately
    let zp = match zp::Encoder::new(buf).provision(24) {
        Complete(enc) => enc,
        Incomplete(_) => panic!(), // XXX
    };
    Start {
        zp,
        array: Box::new(super::MTF_IDENTITY),
        array_inv: Box::new(super::MTF_IDENTITY_INV),
        contexts: Box::new([zp::Context::NEW; NUM_CONTEXTS]),
    }
}

pub struct Start<'enc> {
    zp: zp::Encoder<'enc>,
    array: Box<[Symbol; 256]>,
    array_inv: Box<[u8; 256]>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
}

impl<'enc> Start<'enc> {
    pub fn step<'scratch>(self, data: &[u8], scratch: &'scratch mut Scratch) -> Step<Block<'enc, 'scratch>, (usize, StartSave)> {
        let block_size = data.len() + 1;
        let block_size = if block_size < 1 << 24 {
            block_size as u32
        } else {
            panic!("usage error: length of a BZZ block must be less than `(1<<24) - 1`"); // XXX
        };

        // important: don't BWT until we know encoding can go forward
        // provisioning: 24 decisions for the block size, 2 for the speed
        let mut zp = match self.zp.provision(24 + 2) {
            Complete(enc) => enc,
            Incomplete((off, zp)) => {
                return Incomplete((off, StartSave {
                    zp,
                    array: self.array,
                    array_inv: self.array_inv,
                    contexts: self.contexts,
                }));
            }
        };

        let marker = bwt(data, scratch);
        encode_u24(&mut zp, block_size);
        let speed = match block_size {
            0..=99_999 => {
                zp.encode_passthrough(false);
                Speed::Zero
            }
            100_000..=999_999 => {
                zp.encode_passthrough(true);
                zp.encode_passthrough(false);
                Speed::One
            }
            1_000_000.. => {
                zp.encode_passthrough(true);
                zp.encode_passthrough(true);
                Speed::Two
            }
        };
        let mtf = MtfWithInv::new(speed, self.array, self.array_inv);
        let progress = BlockProgress {
            size: block_size,
            i: 0,
            marker,
            mtf,
            mtf_index: Some(3),
        };

        Complete(Block {
            progress,
            scratch,
            zp,
            contexts: self.contexts,
        })
    }

    pub fn flush(mut self) -> usize {
        // these decisions have already been provisioned, so flushing can be infallible
        encode_u24(&mut self.zp, 0);
        self.zp.flush()
    }
}

pub struct StartSave {
    zp: zp::enc::EncoderSave,
    array: Box<[Symbol; 256]>,
    array_inv: Box<[u8; 256]>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
}

impl StartSave {
    pub fn resume(self, data: &mut [u8]) -> Start<'_> {
        Start {
            zp: self.zp.resume(data),
            array: self.array,
            array_inv: self.array_inv,
            contexts: self.contexts,
        }
    }
}

pub struct Block<'enc, 'scratch> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    zp: zp::Encoder<'enc>,
    progress: BlockProgress,
    scratch: &'scratch mut Scratch,
}

struct BlockProgress {
    size: u32,
    i: u32,
    marker: u32,
    mtf: MtfWithInv,
    mtf_index: Option<u8>,
}

impl<'enc, 'scratch> Block<'enc, 'scratch> {
    pub fn step(self) -> Step<Start<'enc>, (usize, BlockSave<'scratch>)> {
        let Self { mut contexts, mut zp, mut progress, scratch } = self;
        while progress.i < progress.size {
            zp = match zp.provision(16) {
                Complete(enc) => enc,
                Incomplete((off, zp)) => {
                    return Incomplete((off, BlockSave {
                        contexts,
                        zp,
                        progress,
                        scratch,
                    }));
                }
            };

            let symbol = Symbol(scratch.shadow[progress.i as usize]);
            let next = if progress.i == progress.marker {
                256
            } else {
                progress.mtf.get_inv(symbol) as usize
            };
            let start = progress.mtf_index.map_or(256, usize::from).min(2);
            (|| {
                let decision = next == 0;
                zp.encode(decision, &mut contexts[start]);
                if decision { return }
                let decision = next == 1;
                zp.encode(decision, &mut contexts[start + 3]);
                if decision { return }
                for s in 1..8 {
                    let decision = next < 1 << (s + 1);
                    zp.encode(decision, &mut contexts[4 + (1 << s)]);
                    if decision {
                        encode_u8(&mut zp, 5 + (1 << s), s, next as u8 - (1 << s), &mut contexts);
                        return;
                    }
                }
            })();
            progress.mtf_index = next.try_into().ok();
            if let Some(index) = progress.mtf_index {
                progress.mtf.do_rotation(index, symbol);
            }
            progress.i += 1;
        }

        // the caller may decide to flush after this block,
        // and we want `flush` to be infallible, so we provision
        // the required decisions eagerly
        zp = match zp.provision(24) {
            Complete(enc) => enc,
            Incomplete((off, zp)) => {
                return Incomplete((off, BlockSave {
                    contexts,
                    zp,
                    progress,
                    scratch,
                }));
            }
        };

        let (array, array_inv) = progress.mtf.into_inner();
        Complete(Start {
            zp,
            array,
            array_inv,
            contexts,
        })
    }
}

pub struct BlockSave<'scratch> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress,
    zp: zp::enc::EncoderSave,
    scratch: &'scratch mut Scratch,
}

impl<'scratch> BlockSave<'scratch> {
    pub fn resume<'enc>(self, data: &'enc mut [u8]) -> Block<'enc, 'scratch> {
        Block {
            contexts: self.contexts,
            progress: self.progress,
            zp: self.zp.resume(data),
            scratch: self.scratch,
        }
    }
}

fn estimate_compressed_size(plain: &[u8]) -> usize {
    plain.len() / 5 // XXX
}

pub fn compress(plain: &[u8], out: &mut Vec<u8>, scratch: &mut Scratch, split: Split) {
    use core::mem::ManuallyDrop;

    fn grow(out: &mut Vec<u8>, off: usize) -> &mut [u8] {
        // XXX proper growth strategy
        let prev_len = out.len();
        out.resize(prev_len + 1000, 0x00);
        &mut out[off..]
    }

    let mut written = out.len();
    out.resize(written + 52 + estimate_compressed_size(plain), 0x00);
    // XXX use of ManuallyDrop is a hack, necessary because the drop check has a blind
    // spot that's exposed by the loop here
    // TODO find a way to write this kind of encoder loops without hacks
    let mut start = ManuallyDrop::new(start(&mut out[written..]));
    for blk in split.iter(plain) {
        let mut block = loop {
            start = match ManuallyDrop::into_inner(start).step(blk, scratch) {
                Complete(enc) => break ManuallyDrop::new(enc),
                Incomplete((off, save)) => {
                    written += off;
                    ManuallyDrop::new(save.resume(grow(out, off)))
                }
            };
        };
        start = loop {
            block = match ManuallyDrop::into_inner(block).step() {
                Complete(enc) => break ManuallyDrop::new(enc),
                Incomplete((off, save)) => {
                    written += off;
                    ManuallyDrop::new(save.resume(grow(out, off)))
                }
            };
        };
    }
    written += ManuallyDrop::into_inner(start).flush();
    out.truncate(written);
}

pub fn compress_oneshot(plain: &[u8], split: Split) -> Vec<u8> {
    let mut out = Vec::new();
    let mut scratch = Scratch::new();
    compress(plain, &mut out, &mut scratch, split);
    out
}

/// A strategy for dividing input into blocks for compression.
#[derive(Default)]
#[non_exhaustive]
pub enum Split {
    /// The implementation's default strategy.
    #[default]
    Default,
    /// Use blocks of a fixed size, which must be less than `(1 << 24) - 1`.
    Size(usize),
}

impl Split {
    fn iter(self, plain: &[u8]) -> Box<dyn Iterator<Item = &[u8]> + '_> {
        match self {
            Self::Default => Box::new(plain.chunks(1000)), // XXX
            Self::Size(z) => Box::new(plain.chunks(z)),
        }
    }
}
