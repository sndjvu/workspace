//! BZZ encoding.
//!
//! The order of operations is
//!
//! 1. Burrows-Wheeler transform
//! 2. encode block header
//! 3. encode block
//! 4. GOTO 1
//!
//! We separate 1-2 from 3-4 to permit overlapping; it also just makes
//! the code clearer. We end up with two "stages", whereas `crate::bzz::enc`
//! has three, because in that case "decode block header" and
//! "inverse Burrows-Wheeler transform" are not successive, and we still need
//! the block header to be a separate stage to allow the caller to manage
//! memory allocation.

use crate::{Update, zp};
use super::{Scratch, Speed, MtfWithInv, Symbol, NUM_CONTEXTS};
use alloc::boxed::Box;
use alloc::vec::Vec;
use core::cmp::Ordering;

fn find_difference(left: &[u8], right: &[u8]) -> usize {
    left.iter().zip(right).take_while(|(&l, &r)| l == r).count()
}

pub(super) fn bwt(input: &[u8], scratch: &mut Scratch) -> u32 {
    let Scratch { ref mut shadow, ranks: ref mut shifts, .. } = *scratch;

    shadow.clear();
    shadow.extend_from_slice(input);
    shadow.push(0x00); // fake EOB

    shifts.clear();
    shifts.extend(0..=input.len() as u32);
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

pub struct Encoder<'a> {
    zp: zp::Encoder<'a>,
    array: Box<[Symbol; 256]>,
    array_inv: Box<[u8; 256]>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
}

impl<'a> Encoder<'a> {
    pub fn new(buf: &'a mut [u8]) -> Self {
        Self {
            zp: zp::Encoder::new(buf),
            array: Box::new(super::MTF_IDENTITY),
            array_inv: Box::new(super::MTF_IDENTITY_INV),
            contexts: Box::new([zp::Context::NEW; NUM_CONTEXTS]),
        }
    }

    pub fn block<'b>(self, data: &[u8], scratch: &'b mut Scratch) -> Update<EncodeBlock<'a, 'b>, (usize, EncoderSave)> {
        let block_size = data.len() + 1;
        let block_size = if block_size < 1 << 24 {
            block_size as u32
        } else {
            panic!("usage error"); // XXX
        };

        // important: don't BWT until we know encoding can go forward
        let mut zp = match self.zp.provision(24 + 2 + 24) {
            Update::Success(enc) => enc,
            Update::Suspend((off, zp)) => {
                return Update::Suspend((off, EncoderSave {
                    zp,
                    array: self.array,
                    array_inv: self.array_inv,
                    contexts: self.contexts,
                }));
            }
        };

        let marker = bwt(data, scratch);
        encode_u24(&mut zp, block_size);
        let speed = Speed::Zero; // XXX
        match speed {
            Speed::Zero => zp.encode_passthrough(false),
            Speed::One => {
                zp.encode_passthrough(true);
                zp.encode_passthrough(false);
            }
            Speed::Two => {
                zp.encode_passthrough(true);
                zp.encode_passthrough(true);
            }
        }
        let mtf = MtfWithInv::new(speed, self.array, self.array_inv);
        let progress = BlockProgress {
            size: block_size,
            i: 0,
            marker,
            mtf,
            mtf_index: Some(3),
        };

        Update::Success(EncodeBlock {
            progress,
            scratch,
            zp,
            contexts: self.contexts,
        })
    }

    pub fn flush(mut self) -> usize {
        encode_u24(&mut self.zp, 0);
        self.zp.flush()
    }
}

pub struct EncoderSave {
    zp: zp::enc::EncoderSave,
    array: Box<[Symbol; 256]>,
    array_inv: Box<[u8; 256]>,
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
}

impl EncoderSave {
    pub fn resume(self, data: &mut [u8]) -> Encoder<'_> {
        Encoder {
            zp: self.zp.resume(data),
            array: self.array,
            array_inv: self.array_inv,
            contexts: self.contexts,
        }
    }
}

pub struct EncodeBlock<'a, 'b> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    zp: zp::Encoder<'a>,
    progress: BlockProgress,
    scratch: &'b mut Scratch,
}

struct BlockProgress {
    size: u32,
    i: u32,
    marker: u32,
    mtf: MtfWithInv,
    mtf_index: Option<u8>,
}

impl<'a, 'b> EncodeBlock<'a, 'b> {
    pub fn encode(self) -> Update<Encoder<'a>, (usize, EncodeBlockSave<'b>)> {
        let Self { mut contexts, mut zp, mut progress, scratch } = self;
        while progress.i < progress.size {
            zp = match zp.provision(16) {
                Update::Success(enc) => enc,
                Update::Suspend((off, zp)) => {
                    return Update::Suspend((off, EncodeBlockSave {
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
        let (array, array_inv) = progress.mtf.into_inner();
        Update::Success(Encoder {
            zp,
            array,
            array_inv,
            contexts,
        })
    }
}

pub struct EncodeBlockSave<'b> {
    contexts: Box<[zp::Context; NUM_CONTEXTS]>,
    progress: BlockProgress,
    zp: zp::enc::EncoderSave,
    scratch: &'b mut Scratch,
}

impl<'b> EncodeBlockSave<'b> {
    pub fn resume<'a>(self, data: &'a mut [u8]) -> EncodeBlock<'a, 'b> {
        EncodeBlock {
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

pub fn compress(plain: &[u8], out: &mut Vec<u8>, scratch: &mut Scratch, blocks: Blocks) {
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
    let mut encoder = ManuallyDrop::new(Encoder::new(&mut out[written..]));
    for block in blocks.split(plain) {
        let mut block_encoder = loop {
            encoder = match ManuallyDrop::into_inner(encoder).block(block, scratch) {
                Update::Success(blk) => break ManuallyDrop::new(blk),
                Update::Suspend((off, save)) => {
                    written += off;
                    ManuallyDrop::new(save.resume(grow(out, off)))
                }
            };
        };
        encoder = loop {
            block_encoder = match ManuallyDrop::into_inner(block_encoder).encode() {
                Update::Success(enc) => break ManuallyDrop::new(enc),
                Update::Suspend((off, save)) => {
                    written += off;
                    ManuallyDrop::new(save.resume(grow(out, off)))
                }
            };
        };
    }
    written += ManuallyDrop::into_inner(encoder).flush();
    out.truncate(written);
}

pub fn compress_oneshot(plain: &[u8], blocks: Blocks) -> Vec<u8> {
    let mut out = Vec::new();
    let mut scratch = Scratch::new();
    compress(plain, &mut out, &mut scratch, blocks);
    out
}

/// A strategy for dividing data into blocks for compression.
#[derive(Default)]
#[non_exhaustive]
pub enum Blocks {
    /// The implementation's default strategy.
    ///
    /// This is not guaranteed to do anything in particular.
    #[default]
    Default,
    /// Split the input data into zero or more blocks of fixed size, possibly followed by a shorter final
    /// block.
    Size(usize),
}

impl Blocks {
    fn split(self, plain: &[u8]) -> Box<dyn Iterator<Item = &[u8]> + '_> {
        match self {
            Self::Default => Box::new(plain.chunks(1000)), // XXX
            Self::Size(z) => Box::new(plain.chunks(z)),
        }
    }
}
