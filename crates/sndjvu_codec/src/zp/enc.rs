use crate::Step::{self, *};
use super::{Context, Entry};
use core::mem::take;
use core::cell::Cell;

struct State {
    a: u32,
    u: u32,
}

struct Out<T = ()> {
    delay: u32,
    buffer: u32,
    run_counter: u32,
    ready: u8,
    dispense_countdown: u32,

    place: T,
}

impl<T> Out<T> {
    fn relay<U>(self, place: U) -> Out<U> {
        Out {
            delay: self.delay,
            buffer: self.buffer,
            run_counter: self.run_counter,
            ready: self.ready,
            dispense_countdown: self.dispense_countdown,
            place,
        }
    }
}

struct Place<'a> {
    inner: core::slice::Iter<'a, Cell<u8>>,
    off: usize,
}

impl<'a> Out<Place<'a>> {
    fn dispense(&mut self) {
        // FIXME better panic message (mention provisioning)
        self.place.inner.next().unwrap().set(take(&mut self.ready));
        self.place.off += 1;
        self.dispense_countdown = 8;
    }

    fn bit(&mut self, b: bool) {
        if self.delay > 0 {
            self.delay -= 1;
            return;
        }
        self.ready = (self.ready << 1) | (b as u8);
        self.dispense_countdown -= 1;
        if self.dispense_countdown == 0 {
            self.dispense();
        }
    }

    fn run(&mut self, head: bool) {
        self.bit(head);
        for _ in 0..self.run_counter {
            self.bit(!head);
        }
        self.run_counter = 0;
    }

    fn emit(&mut self, u: u32) {
        let b = 1u32.wrapping_sub(u >> 15);
        let buf = (self.buffer << 1).wrapping_add(b);
        self.buffer = buf & 0xff_ff_ff;
        match buf >> 24 {
            0x01 => self.run(true),
            0xff => self.run(false),
            0x00 => self.run_counter += 1,
            _ => unreachable!(),
        }
    }

    fn can(&self, num_decisions: u32) -> bool {
        // bits that we've already committed to
        let mut bits = self.run_counter;
        // at most 16 bits per decision
        bits += 16 * num_decisions;
        // at most 26 bits for flush (empirical)
        bits += 24 + 2;
        // the first N bits will be used to fill a partial byte that's accounted separately
        bits -= self.dispense_countdown;
        // round up, plus the partial byte previously mentioned
        let bytes = 1 + (bits + 7) / 8;
        bytes as usize <= self.place.inner.as_slice().len()
    }

    fn fill(mut self) -> usize {
        while self.dispense_countdown < 8 {
            self.bit(true);
        }
        self.place.off
    }
}

pub struct Encoder<'a> {
    state: State,
    out: Out<Place<'a>>,
    #[cfg(debug_assertions)] remaining: u32,
}

pub struct EncoderSave {
    state: State,
    out: Out,
}

impl EncoderSave {
    pub fn resume(self, buf: &[Cell<u8>]) -> Encoder<'_> {
        Encoder {
            state: self.state,
            out: self.out.relay(Place { inner: buf.iter(), off: 0 }),
            #[cfg(debug_assertions)] remaining: 0,
        }
    }
}

const HALF: u32 = 0x80_00;
const THREE_EIGHTHS: u32 = 0x60_00;
const ONE: u32 = 0x1_00_00;

impl<'a> Encoder<'a> {
    pub fn new(data: &'a [Cell<u8>]) -> Self {
        Self {
            state: State { a: 0, u: 0 },
            out: Out {
                delay: 25,
                buffer: 0xff_ff_ff,
                run_counter: 0,
                ready: 0,
                dispense_countdown: 8,

                place: Place { inner: data.iter(), off: 0 },
            },
            #[cfg(debug_assertions)] remaining: 0,
        }
    }

    pub fn provision(mut self, num_decisions: u32) -> Step<Self, (usize, EncoderSave)> {
        if self.out.can(num_decisions) {
            #[cfg(debug_assertions)] {
                self.remaining = num_decisions;
            }
            Complete(self)
        } else {
            let off = self.out.place.off;
            let save = EncoderSave {
                state: self.state,
                out: self.out.relay(()),
            };
            Incomplete((off, save))
        }
    }

    pub fn encode(&mut self, decision: bool, context: &mut Context) {
        #[cfg(debug_assertions)] {
            self.remaining = self.remaining.checked_sub(1).expect("TODO");
        }

        let Entry { Δ, θ, μ, λ } = context.entry();
        let mps = context.mps();

        let State { mut a, mut u } = self.state;
        let z_0 = a + Δ as u32;
        if decision == mps && z_0 < HALF {
            self.state.a = z_0;
            return;
        }
        let d = THREE_EIGHTHS + (z_0 + a) / 4;
        let z = z_0.min(d);
        if decision != mps {
            context.k = λ;

            a = a + ONE - z;
            u = u + ONE - z;
            while a >= HALF {
                self.out.emit(u);
                a = (a << 1) & 0xff_ff;
                u = (u << 1) & 0xff_ff;
            }
        } else {
            if a >= θ as u32 {
                context.k = μ;
            }

            a = z;
            if a >= HALF {
                self.out.emit(u);
                a = (a << 1) & 0xff_ff;
                u = (u << 1) & 0xff_ff;
            }
        }
        self.state = State { a, u };
    }

    pub fn encode_passthrough(&mut self, decision: bool) {
        #[cfg(debug_assertions)] {
            self.remaining = self.remaining.checked_sub(1).expect("TODO");
        }

        let State { mut a, mut u } = self.state;
        let z = HALF + a / 2;
        if decision {
            a = a + ONE - z;
            u = u + ONE - z;
            while a >= HALF {
                self.out.emit(u);
                a = (a << 1) & 0xff_ff;
                u = (u << 1) & 0xff_ff;
            }
        } else {
            a = z;
            if a >= HALF {
                self.out.emit(u);
                a = (a << 1) & 0xff_ff;
                u = (u << 1) & 0xff_ff;
            }
        }
        self.state = State { a, u };
    }

    // TODO this crosses layers in an uncomfortable way, try to refactor it
    pub fn flush(mut self) -> usize {
        if self.state.u > HALF {
            self.state.u = ONE;
        } else if self.state.u > 0 {
            self.state.u = HALF;
        }
        while self.out.buffer != 0xff_ff_ff || self.state.u != 0 {
            self.out.emit(self.state.u);
            self.state.u = (self.state.u << 1) & 0xff_ff;
        }
        self.out.run(true);
        self.out.fill()
    }
}

