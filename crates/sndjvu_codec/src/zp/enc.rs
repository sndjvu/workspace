use crate::Update;
use super::{Context, Entry};
use crate::Progress;
use core::mem::{take, replace};

struct State {
    a: u16,
    u: u16,
}

struct Out<T = ()> {
    delay: u32,
    shadow: u64,
    run_counter: u32,
    ready: u64,
    dispense_countdown: u32,

    place: T,
}

struct Place<'a> {
    pos: usize,
    full: &'a mut [u8],
}

impl<'a> Out<Place<'a>> {
    fn dispense(&mut self) {
        let pos = self.place.pos;
        let front = &mut self.place.full[pos..pos + 8];
        let front: &mut [u8; 8] = front.try_into().unwrap();
        *front = take(&mut self.ready).to_be_bytes();
        self.place.pos += 8;
    }

    fn bit(&mut self, b: bool) {
        if self.dispense_countdown == 0 {
            self.dispense();
            self.dispense_countdown = 64;
        }
        self.ready = (self.ready << 1) | (b as u64);
        self.dispense_countdown -= 1;
    }

    fn run(&mut self, head: bool) {
        // TODO some kind of fast path/unrolling
        self.bit(head);
        for _ in 0..take(&mut self.run_counter) {
            self.bit(!head);
        }
    }

    fn emit(&mut self, u: u16, n: u32) {
        todo!()
    }

    fn emit_special(&mut self, up: u32) {
        todo!()
    }

    fn can(&self, num_decisions: u32) -> bool {
        todo!()
    }
}

pub struct Encoder<'a> {
    state: State,
    out: Out<Place<'a>>,
}

pub struct EncoderSave {
    state: State,
    out: Out,
}

impl EncoderSave {
    pub fn resume(self, buf: &mut [u8]) -> Encoder<'_> {
        todo!()
    }
}

const HALF: u32 = 0x80_00;
const THREE_EIGHTHS: u32 = 0x60_00;
const ONE: u32 = 0x1_00_00;

impl<'a> Encoder<'a> {
    pub fn provision(self, num_decisions: u32) -> Update<Self, Progress<'a, EncoderSave>> {
        if self.out.can(num_decisions) {
            Update::Success(self)
        } else {
            todo!()
        }
    }

    pub fn encode(&mut self, decision: bool, context: &mut Context) {
        let Entry { Δ, θ, μ, λ } = context.entry();
        let mps = context.mps();

        let (a, u) = (self.state.a as u32, self.state.u as u32);
        let z_0 = a + Δ as u32;
        if decision == mps && z_0 < HALF {
            self.state.a = z_0 as u16;
            return;
        }
        let d = THREE_EIGHTHS + (z_0 + a) / 4;
        let z = z_0.min(d);
        if decision != mps {
            context.k = λ;

            // we always emit at least once on this path
            let ap = a + ONE - z;
            let up = u + ONE - z;
            debug_assert!(ap >= HALF);
            self.out.emit_special(up);
            let a = (ap << 1) as u16;
            let u = (up << 1) as u16;
            let n = a.leading_ones();
            self.out.emit(u, n);
            self.state.a = a.checked_shl(n).unwrap_or(0);
            self.state.u = u.checked_shl(n).unwrap_or(0);
        } else {
            if a >= θ as u32 {
                context.k = μ;
            }

            // we emit at most once on this path
            let a = z as u16;
            let u = u as u16;
            let n = (z >= HALF) as u32;
            self.out.emit(u, n);
            self.state.a = a << n;
            self.state.u = u << n;
        }
    }

    pub fn encode_passthrough(&mut self, decision: bool) {
        let (a, u) = (self.state.a as u32, self.state.u as u32);
        let z = HALF + a / 2;
        if decision {
            let ap = a + ONE - z;
            let up = a + ONE - z;
            debug_assert!(ap >= HALF);
            self.out.emit_special(up);
            let a = (ap << 1) as u16;
            let u = (up << 1) as u16;
            let n = a.leading_ones();
            self.out.emit(u, n);
            self.state.a = a.checked_shl(n).unwrap_or(0);
            self.state.u = u.checked_shl(n).unwrap_or(0);
        } else {
            let a = z as u16;
            let u = u as u16;
            let n = (z >= HALF) as u32;
            self.out.emit(u, n);
            self.state.a = a << n;
            self.state.u = u << n;
        }
    }

    // end of encoding: write any residual bytes
    pub fn flush(self) -> Progress<'a> {
        todo!()
    }
}

