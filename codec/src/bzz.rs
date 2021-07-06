// sndjvu_codec::bzz
// Copyright (C) 2021 Cole Miller
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

/*! Implements BZZ, a general-purpose compression algorithm.

BZZ is used by the DjVu format to code various parts of a document, including text.
*/

// The spec says there are either 260 or 262 decoding contexts. But DjVuLibre uses an array
// of 300 contexts...
const NUM_CONTEXTS: usize = 300;

pub struct Scratch(Vec<(u8, u32)>);

impl Scratch {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

pub mod dec {
    use super::{Scratch, NUM_CONTEXTS};
    use crate::zp;
    #[allow(unused)]
    use std::mem::{replace, swap, take};

    fn decode_u24_raw(source: &mut zp::Source<impl zp::dec::Input>) -> u32 {
        let mut n = 1;
        while n < 1 << 24 {
            n = (n << 1) | source.decode_decision_passthrough() as u32;
        }
        n - (1 << 24)
    }

    pub struct Decoder<I> {
        contexts: [zp::Context; NUM_CONTEXTS],
        source: zp::Source<I>,
    }

    impl<I: zp::dec::Input> Decoder<I> {
        pub fn new(source: zp::Source<I>) -> Self {
            const NEW_CONTEXT: zp::Context = zp::Context::new();
            Self {
                contexts: [NEW_CONTEXT; 300],
                source,
            }
        }

        fn decode_decision_with(&mut self, index: usize) -> bool {
            let Self { contexts, source } = self;
            source.decode_decision(&mut contexts[index])
        }

        fn decode_u8_with(&mut self, offset: usize, num_bits: u32) -> u8 {
            assert!((1..8).contains(&num_bits), "decoder bug");
            let Self { contexts, source } = self;
            let mut n = 1;
            while n < 1 << num_bits {
                n = (n << 1) | source.decode_decision(&mut contexts[offset + n as usize - 1]) as u8;
            }
            n - (1 << num_bits)
        }

        pub fn decode_and_append_block(
            &mut self,
            dest: &mut Vec<u8>,
            scratch: &mut Scratch,
        ) -> Option<u32> {
            const MTF_IDENTITY: [u8; 256] = {
                let mut a = [0; 256];
                let mut z: usize = 0;
                let mut j: u8 = 0;
                while z < 256 {
                    a[z] = j;
                    z += 1;
                    j = j.wrapping_add(1);
                }
                a
            };

            /* Phase 1: arithmetic decoding */

            let block_size = decode_u24_raw(&mut self.source);
            if block_size == 0 {
                return None;
            }
            let orig_len = dest.len();
            dest.resize_with(orig_len + block_size as usize, Default::default);
            let dest_slice = &mut dest[orig_len..];
            scratch.0.resize_with(block_size as usize, Default::default);

            #[rustfmt::skip]
            let speed: u32 = if self.source.decode_decision_passthrough() {
                if self.source.decode_decision_passthrough() { 2 } else { 1 }
            } else {
                0
            };

            let mut mtf = MTF_IDENTITY;
            let mut mtf_index: usize = 3;
            let mut marker: Option<u32> = None;
            let mut accumulator: u32 = 4;
            let mut freqs: [u32; 4] = [0; 4];
            for i in 0..block_size {
                let ctx_index = mtf_index.min(2);
                if self.decode_decision_with(ctx_index) {
                    mtf_index = 0;
                } else if self.decode_decision_with(ctx_index + 3) {
                    mtf_index = 1;
                } else if let Some(x) =
                    (1_u32..8).find(|&s| self.decode_decision_with(4 + (1 << s)))
                {
                    mtf_index = (1 << x) + self.decode_u8_with(5 + (1 << x), x) as usize;
                } else {
                    mtf_index = 256; // XXX
                    dest_slice[i as usize] = 0;
                    marker = Some(i);
                    continue;
                }
                dest_slice[i as usize] = mtf[mtf_index];

                accumulator += accumulator >> speed;
                if accumulator > 0x10_00_00_00 {
                    accumulator >>= 24;
                    for f in &mut freqs {
                        *f >>= 24;
                    }
                }
                let threshold = accumulator + freqs.get(mtf_index).copied().unwrap_or(0);
                #[rustfmt::skip]
                let stop = freqs.iter().enumerate().rev()
                    .find(|&(_, &freq)| freq > threshold)
                    .map(|(k, _)| k + 1)
                    .unwrap_or(0);
                mtf[stop..=mtf_index].rotate_right(1);
                mtf[stop] = dest_slice[i as usize];
                freqs[stop..].rotate_right(1);
                freqs[stop] = threshold;
                assert!(
                    freqs[0] >= freqs[1] && freqs[1] >= freqs[2] && freqs[2] >= freqs[3],
                    "decoder bug"
                );
            }

            /* Phase 2: inverse Burrows–Wheeler transform */

            let marker = marker.expect("decoder bug");
            assert!((1..block_size).contains(&marker), "decoder bug");
            let mut counts = [0_u32; 256];

            for (i, &x) in dest_slice.iter().enumerate() {
                scratch.0[i] = if i == marker as usize {
                    (x, 0)
                } else {
                    let old = counts[x as usize];
                    counts[x as usize] += 1;
                    (x, old)
                }
            }

            let mut last = 1;
            for n in &mut counts {
                last += replace(n, last);
            }
            assert_eq!(last, block_size, "decoder bug");

            let mut k = 0_u32;
            for i in (0..block_size - 1).rev() {
                let (x, n) = scratch.0[k as usize];
                dest_slice[i as usize] = x;
                k = counts[x as usize] + n;
            }
            assert_eq!(k, marker, "decoder bug");

            dest.pop();
            scratch.0.clear();
            Some(block_size - 1)
        }
    }

    pub struct Buffer {
        inner: Vec<u8>,
    }

    impl Buffer {
        pub fn new() -> Self {
            Self { inner: Vec::new() }
        }

        pub fn into_inner(mut self) -> Vec<u8> {
            self.inner.clear();
            self.inner
        }

        pub fn decode_to_end(
            &mut self,
            source: zp::Source<impl zp::dec::Input>,
            scratch: &mut Scratch,
        ) -> &[u8] {
            let mut decoder = Decoder::new(source);
            self.inner.clear();
            while decoder
                .decode_and_append_block(&mut self.inner, scratch)
                .is_some()
            {}
            &self.inner
        }

        pub fn try_for_each_block<E>(
            &mut self,
            source: zp::Source<impl zp::dec::Input>,
            scratch: &mut Scratch,
            mut f: impl for<'a> FnMut(&'a [u8]) -> Result<(), E>,
        ) -> Result<(), E> {
            let mut decoder = Decoder::new(source);
            while {
                self.inner.clear();
                decoder
                    .decode_and_append_block(&mut self.inner, scratch)
                    .is_some()
            } {
                f(&self.inner)?;
            }
            Ok(())
        }
    }

    impl sndjvu_waist::BzzBuffer for Buffer {
        type Scratch = Scratch;

        fn decode_bytes(&mut self, bytes: &[u8], scratch: &mut Self::Scratch) -> &[u8] {
            self.decode_to_end(
                zp::Source::new(zp::BytesInput::new(bytes.iter().copied())),
                scratch,
            )
        }
    }
}
