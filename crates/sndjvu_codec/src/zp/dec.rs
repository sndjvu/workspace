use crate::Step::{self, *};
use super::{Context, Entry};
use core::ops::{Add, Div, Mul, Sub};
use core::mem::take;
use alloc::vec::Vec;

struct State {
    // the 2 most-significant bytes are djvulibre's `code`
    // the 6 least-significant bytes are like djvulibre's `buffer`
    // provided that enough bits have been loaded, shifting bits
    // from the buffer to the code is as simple as left-shifting
    // this `c`
    c: u64,
    // this is equal to 16 minus the number of "good" bits in c_lo
    // thus, before every decoding operation we must check that
    // level <= 0, and when shifting `c` we *increase* the level
    // by that amount
    level: i32,
    // as in djvlibre
    fence: u16,
    // as in djvulibre
    a: u16,
}

impl State {
    // the argument is the first two bytes of input interpreted as a big-endian u16
    fn new(c: u16) -> Self {
        let mut it = Self {
            c: (c as u64) << 48,
            // we haven't done a load yet, so the number of good bits in c_lo is 0
            level: 16,
            fence: 0,
            a: 0,
        };
        it.update_fence();
        it
    }

    fn update_fence(&mut self) {
        self.fence = (self.c >> 48).min(0x7f_ff) as u16;
    }

    fn decode(&mut self, context: &mut Context) -> bool {
        let Entry { Δ, θ, μ, λ } = context.entry();
        let mps = context.mps();

        let z_0 = self.a as u64 + Δ as u64;
        if z_0 <= self.fence as u64 {
            self.a = z_0 as u16;
            return mps;
        }
        let d = 0x60_00 + (z_0 + self.a as u64) / 4;
        let z = z_0.min(d);
        assert!(z <= u16::MAX as u64); // FIXME possible to prove that this will never fail?
        let z = z as u16;
        if self.c >> 48 < z as u64 {    // LPS path
            context.k = λ;
            let (a, c) = (
                self.a.wrapping_sub(z),
                self.c.wrapping_sub((z as u64) << 48),
            );
            let n = a.leading_ones();
            self.a = a.checked_shl(n).unwrap_or(0);
            self.c = c << n;
            self.update_fence();
            self.level += n as i32;

            !mps
        } else {                        // MPS path
            if self.a >= θ {
                context.k = μ;
            }
            let (a, c) = (z, self.c);
            self.a = a << 1;
            self.c = c << 1;
            self.update_fence();
            self.level += 1;

            mps
        }
    }

    fn decode_passthrough(&mut self) -> bool {
        let z = 0x80_00 + (self.a >> 1);
        if self.c >> 48 < z as u64 {    // LPS path
            let (a, c) = (
                self.a.wrapping_sub(z),
                self.c.wrapping_sub((z as u64) << 48),
            );
            let n = a.leading_ones();
            self.a = a.checked_shl(n).unwrap_or(0);
            self.c = c << n;
            self.update_fence();
            self.level += n as i32;
            
            true
        } else {                        // MPS path
            let (a, c) = (z, self.c);
            self.a = a << 1;
            self.c = c << 1;
            self.update_fence();
            self.level += 1;

            false
        }
    }

    // the argument is the next 4 bytes of input, interpreted as a big-endian u32
    fn reload(&mut self, b: u32) {
        // this line is why we defined level as we did
        // for instance, if before the reload level = 1
        // then there are 15 good bits and we shift left
        // by 1 to fill the 1 invalid bit
        self.c |= (b as u64) << self.level;
        self.level -= 32;
    }
}

pub struct DecoderSave {
    state: State,
    plank: Vec<u8>,
    pos: usize,
}

impl DecoderSave {
    pub fn resume(self, buf: &[u8]) -> Decoder<'_> {
        let marker = self.plank.len();
        Decoder {
            state: self.state,
            pos: self.pos,
            input: Input::InPlank {
                plank: self.plank,
                marker,
                buf,
            },
            #[cfg(debug_assertions)] remaining: 0,
        }
    }

    pub fn seal<'a>(self) -> Decoder<'a> {
        Decoder {
            state: self.state,
            pos: self.pos,
            input: Input::Overrun { plank: self.plank },
            #[cfg(debug_assertions)] remaining: 0,
        }
    }

    #[allow(unused)]
    pub fn seal_provision<'a>(self, num_decisions: u32) -> Decoder<'a> {
        match self.seal().provision(num_decisions) {
            Complete(dec) => dec,
            Incomplete(_) => unreachable!(),
        }
    }
}

enum Input<'a> {
    InBuf {
        buf: &'a [u8],
        saved_plank: Vec<u8>,
    },
    InPlank {
        plank: Vec<u8>,
        marker: usize,
        buf: &'a [u8],
    },
    Overrun {
        plank: Vec<u8>,
    },
}

pub struct Decoder<'a> {
    state: State,
    pos: usize,
    input: Input<'a>,
    #[cfg(debug_assertions)] remaining: u32,
}

impl<'a> Decoder<'a> {
    fn upcoming_bytes(&self) -> &[u8] {
        match self.input {
            Input::InBuf { buf, .. } => &buf[self.pos..],
            Input::InPlank { ref plank, .. } | Input::Overrun { ref plank } => &plank[self.pos..],
        }
    }

    fn reload(&mut self) {
        // TODO experiment with a u128 register
        // which would allow loading 12 bytes at a time (16 - 2 - 2)
        let mut b = [0; 4];
        // FIXME panic message should mention provisioning
        b.copy_from_slice(&self.upcoming_bytes()[..4]);
        self.pos += 4;
        self.state.reload(u32::from_be_bytes(b));
    }

    // FIXME is this correct?
    pub fn new(buf: &'a [u8]) -> Self {
        let (front, pos) = match *buf {
            [] => ([0xff, 0xff], 0),
            [b_0] => ([b_0, 0xff], 1),
            [b_0, b_1, ..] => ([b_0, b_1], 2),
        };
        let c = u16::from_be_bytes(front);
        Self {
            state: State::new(c),
            pos,
            input: Input::InBuf { buf, saved_plank: Vec::new() },
            #[cfg(debug_assertions)] remaining: 0,
        }
    }

    pub fn provision(mut self, num_decisions: u32) -> Step<Self, DecoderSave> {
        // check whether we've passed the marker
        if let Input::InPlank { ref mut plank, marker, buf } = self.input {
            if let Some(over) = self.pos.checked_sub(marker) {
                self.pos = over;
                self.input = Input::InBuf { buf, saved_plank: take(plank) };
            }
        }

        #[cfg(debug_assertions)] {
            self.remaining = num_decisions;
        }

        let decisions_avail = 16i32.sub(self.state.level).div(16) as u32;
        let decisions_rem = match num_decisions.checked_sub(decisions_avail) {
            None | Some(0) => return Complete(self),
            Some(n) => n,
        };
        let need = decisions_rem
            .add(1).div(2) // 2 decisions per load, rounding conservatively
            .mul(4) // 4 bytes per load
            as usize;

        match self.input {
            Input::InBuf { buf, ref mut saved_plank } => {
                if buf.get(self.pos..self.pos + need).is_some() {
                    Complete(self)
                } else {
                    let mut plank = take(saved_plank);
                    plank.clear();
                    plank.extend_from_slice(&buf[self.pos..]);
                    Incomplete(DecoderSave {
                        state: self.state,
                        plank,
                        pos: 0,
                    })
                }
            }

            Input::InPlank { ref mut plank, marker, buf } => {
                let have = plank.len() - self.pos;
                let extra = need.saturating_sub(have);
                let start = plank.len() - marker;
                if let Some(slice) = buf.get(start..start + extra) {
                    plank.extend_from_slice(slice);
                    Complete(self)
                } else {
                    let mut plank = take(plank);
                    plank.extend_from_slice(&buf[start..]);
                    Incomplete(DecoderSave {
                        state: self.state,
                        plank,
                        pos: self.pos,
                    })
                }
            }

            Input::Overrun { ref mut plank } => {
                let have = plank.len() - self.pos;
                let extra = need.saturating_sub(have);
                plank.extend(core::iter::repeat(0xff).take(extra));
                Complete(self)
            }
        }
    }

    pub fn decode(&mut self, context: &mut Context) -> bool {
        #[cfg(debug_assertions)] {
            self.remaining = self.remaining.checked_sub(1)
                .expect("tried to decode a decision that wasn't provisioned");
        }
        if self.state.level > 0 {
            self.reload();
        }
        self.state.decode(context)
    }

    pub fn decode_passthrough(&mut self) -> bool {
        #[cfg(debug_assertions)] {
            self.remaining = self.remaining.checked_sub(1)
                .expect("tried to decode a decision that wasn't provisioned");
        }
        if self.state.level > 0 {
            self.reload();
        }
        self.state.decode_passthrough()
    }
}
