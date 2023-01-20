use crate::Step::*;
use super::Scratch;
use proptest::prelude::*;
use std::vec::Vec;
use std::{vec, format};
use std::cell::Cell;

fn compress(data: &[u8], scratch: &mut Scratch) -> Vec<u8> {
    use std::mem::ManuallyDrop;
    use super::enc::*;

    let mut out = vec![0; 4096];
    let mut pos = 0;
    let out_slice = Cell::from_mut(&mut out[pos..]).as_slice_of_cells();
    let mut start = ManuallyDrop::new(start(out_slice));
    for chunk in data.chunks(100) {
        let mut block = loop {
            start = match ManuallyDrop::into_inner(start).step(chunk, scratch) {
                Complete(enc) => break ManuallyDrop::new(enc),
                Incomplete((off, save)) => {
                    pos += off;
                    out.resize(pos + 4096, 0);
                    let out_slice = Cell::from_mut(&mut out[pos..]).as_slice_of_cells();
                    ManuallyDrop::new(save.resume(out_slice))
                }
            };
        };
        start = loop {
            block = match ManuallyDrop::into_inner(block).step() {
                Complete(enc) => break ManuallyDrop::new(enc),
                Incomplete((off, save)) => {
                    pos += off;
                    out.resize(pos + 4096, 0);
                    let out_slice = Cell::from_mut(&mut out[pos..]).as_slice_of_cells();
                    ManuallyDrop::new(save.resume(out_slice))
                }
            };
        };
    }
    let off = ManuallyDrop::into_inner(start).flush();
    out.truncate(pos + off);
    out
}

fn decompress(bzz: &[u8], scratch: &mut Scratch) -> Result<Vec<u8>, super::dec::Error> {
    use std::mem::ManuallyDrop;
    use super::dec::*;

    let mut out = vec![];
    let mut start = ManuallyDrop::new(start(bzz));
    loop {
        let mut block = loop {
            start = match ManuallyDrop::into_inner(start).step(scratch) {
                Complete(None) => return Ok(out),
                Complete(Some(enc)) => break ManuallyDrop::new(enc),
                Incomplete(save) => ManuallyDrop::new(save.seal()),
            };
        };
        let (shuffle, next) = loop {
            block = match ManuallyDrop::into_inner(block).step()? {
                Complete((shuf, enc)) => break (shuf, ManuallyDrop::new(enc)),
                Incomplete(save) => ManuallyDrop::new(save.seal()),
            };
        };
        let pos = out.len();
        out.resize(pos + shuffle.len(), 0);
        shuffle.run(Cell::from_mut(&mut out[pos..]).as_slice_of_cells());
        start = next;
    }
}

proptest! {
    #[test]
    fn bwt_round_trip(input in prop::collection::vec(prop::num::u8::ANY, 1..1_000)) {
        let mut scratch = Scratch::new();
        let marker = super::enc::bwt(&input, &mut scratch);
        let mut buf = vec![0; input.len()];
        super::dec::bwt_inv(marker, Cell::from_mut(&mut buf[..]).as_slice_of_cells(), &mut scratch);
        prop_assert_eq!(input, buf);
    }

    #[test]
    fn round_trip(input in prop::collection::vec(prop::num::u8::ANY, 10..1_000)) {
        let mut scratch = Scratch::new();
        let compressed = compress(&input, &mut scratch);
        let output = decompress(&compressed, &mut scratch).unwrap();
        prop_assert_eq!(output, input);
    }
}
