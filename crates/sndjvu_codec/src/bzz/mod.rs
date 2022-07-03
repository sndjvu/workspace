use alloc::vec::Vec;
use alloc::boxed::Box;

// The spec says there are either 260 or 262 decoding contexts. But DjVuLibre uses an array
// of 300 contexts...
const NUM_CONTEXTS: usize = 300;

#[derive(Clone, Copy)]
enum Speed {
    Zero,
    One,
    Two,
}

#[derive(Clone, Copy, Debug)]
struct Symbol(u8);

impl Symbol {
    fn get(self) -> u8 {
        self.0
    }
}

/// Scratch memory for BZZ encoding or decoding.
/// 
/// A value of this type is opaque, but can be reused for multiple coding operations to save allocations.
#[derive(Debug)]
pub struct Scratch {
    shadow: Vec<u8>,
    counts: Box<[u32; 256]>,
    ranks: Vec<u32>,
}

impl Default for Scratch {
    fn default() -> Self {
        Self::new()
    }
}

impl Scratch {
    pub fn new() -> Self {
        Self {
            shadow: Vec::new(),
            counts: Box::new([0; 256]),
            ranks: Vec::new(),
        }
    }
}

struct Mtf {
    speed: Speed,
    accumulator: u32,
    frequencies: [u32; 4],
    array: Box<[Symbol; 256]>,
}

const MTF_IDENTITY: [Symbol; 256] = {
    let mut a = [Symbol(0); 256];
    let mut j: u8 = 1;
    while j > 0 {
        a[j as usize] = Symbol(j);
        j = j.wrapping_add(1);
    }
    a
};

impl Mtf {
    fn new(speed: Speed, mut array: Box<[Symbol; 256]>) -> Self {
        *array = MTF_IDENTITY;
        Self { speed, accumulator: 4, frequencies: [0; 4], array }
    }

    fn do_rotation_inner(&mut self, index: u8, symbol: Symbol) -> u8 {
        self.accumulator += self.accumulator >> self.speed as u32;
        if self.accumulator > 0x10_00_00_00 {
            self.accumulator >>= 24;
            for freq in &mut self.frequencies {
                *freq >>= 24;
            }
        }

        let threshold = self.accumulator + self.frequencies.get(index as usize).copied().unwrap_or(0);
        let stop = {
            // TODO rewrite this part to use <[_]>::rotate_right
            let mut k = index;
            while k > 3 {
                self.array[k as usize] = self.array[k as usize - 1];
                k -= 1;
            }
            while k > 0 && self.frequencies[k as usize - 1] <= threshold {
                self.array[k as usize] = self.array[k as usize - 1];
                self.frequencies[k as usize] = self.frequencies[k as usize - 1];
                k -= 1;
            }
            k
        };
        self.array[stop as usize] = symbol;
        self.frequencies[stop as usize] = threshold;
        assert!(
            self.frequencies[0] >= self.frequencies[1]
                && self.frequencies[1] >= self.frequencies[2]
                && self.frequencies[2] >= self.frequencies[3]
        );
        stop
    }

    fn do_rotation(&mut self, index: u8) -> Symbol {
        let symbol = self.array[index as usize];
        self.do_rotation_inner(index, symbol);
        symbol
    }

    fn into_inner(self) -> Box<[Symbol; 256]> {
        self.array
    }
}

struct MtfWithInv {
    inner: Mtf,
    inv: Box<[u8; 256]>,
}

const MTF_IDENTITY_INV: [u8; 256] = {
    let mut a = [0; 256];
    let mut j: u8 = 1;
    while j > 0 {
        a[j as usize] = j;
        j = j.wrapping_add(1);
    }
    a
};

impl MtfWithInv {
    fn new(speed: Speed, array: Box<[Symbol; 256]>, mut inv: Box<[u8; 256]>) -> Self {
        *inv = MTF_IDENTITY_INV;
        let inner = Mtf::new(speed, array);
        Self { inner, inv }
    }

    fn get_inv(&self, symbol: Symbol) -> u8 {
        self.inv[symbol.get() as usize]
    }

    fn do_rotation(&mut self, index: u8, symbol: Symbol) {
        let k = self.inner.do_rotation_inner(index, symbol);
        self.inv[symbol.get() as usize] = k;
    }

    fn into_inner(self) -> (Box<[Symbol; 256]>, Box<[u8; 256]>) {
        (self.inner.into_inner(), self.inv)
    }
}

pub mod dec;
pub use dec::{decompress, decompress_oneshot};

pub mod enc;
pub use enc::{compress, compress_oneshot};
