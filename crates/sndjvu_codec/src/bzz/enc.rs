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

fn bwt(input: &[u8], scratch: &mut Scratch) -> u32 {
    todo!()
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
        let mut zp = match self.zp.provision(24 + 2) {
            Update::Success(enc) => enc,
            Update::Suspend(_) => todo!(),
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

        // so that flush can be infallible
        zp = match zp.provision(24) {
            Update::Success(enc) => enc,
            Update::Suspend(_) => todo!(),
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
    pub fn encode(mut self) -> Update<Encoder<'a>, (usize, EncodeBlockSave<'b>)> {
        let Self { mut contexts, mut zp, mut progress, scratch } = self;
        while progress.i < progress.size {
            zp = match zp.provision(16) {
                Update::Success(enc) => enc,
                Update::Suspend(_) => todo!(),
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
    zp: zp::enc::EncoderSave,
    scratch: &'b mut Scratch,
}

fn estimate_compressed_size(plain: &[u8]) -> usize {
    todo!()
}

pub fn compress(plain: &[u8], out: &mut Vec<u8>, scratch: &mut Scratch, blocks: Blocks) {
    let orig_len = out.len();
    out.resize(orig_len + estimate_compressed_size(plain), 0x00);
    let mut encoder = Encoder::new(&mut out[orig_len..]);
    for block in blocks.split(plain) {
        // XXX this loop is gross
        loop {
            let block_enc = match encoder.block(block, scratch) {
                Update::Success(enc) => enc,
                Update::Suspend(_) => todo!(),
            };
            encoder = match block_enc.encode() {
                Update::Success(enc) => enc,
                Update::Suspend(_) => todo!(),
            };
            break;
        }
    }
    let off = encoder.flush();
    out.truncate(orig_len + off);
}

pub fn compress_oneshot(plain: &[u8], blocks: Blocks) -> Vec<u8> {
    let mut out = Vec::new();
    let mut scratch = Scratch::new();
    compress(plain, &mut out, &mut scratch, blocks);
    out
}

/// A strategy for dividing data into blocks for compression.
#[non_exhaustive]
pub enum Blocks {
    /// The implementation's default strategy.
    ///
    /// This is not guaranteed to do anything in particular.
    Default,
    /// Split the input data into zero or more blocks of fixed size, followed by a shorter final
    /// block.
    Size(usize),
}

impl Blocks {
    fn split(self, plain: &[u8]) -> Box<dyn Iterator<Item = &[u8]> + '_> {
        match self {
            Self::Default => todo!(),
            Self::Size(z) => Box::new(plain.chunks(z)),
        }
    }
}
