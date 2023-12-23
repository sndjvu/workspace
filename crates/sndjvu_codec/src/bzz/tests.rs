use crate::{Step::*, cells};
use super::Scratch;
use proptest::prelude::*;
use std::vec::Vec;
use std::{vec, format};

fn compress(data: &[u8], scratch: &mut Scratch) -> Vec<u8> {
    use super::enc::*;

    let mut out = vec![0; 4096];
    let mut pos = 0;
    let mut start = start(cells(&mut out[pos..]));
    for chunk in data.chunks(100) {
        let mut block = loop {
            start = match start.step(chunk, scratch) {
                Complete(enc) => break enc,
                Incomplete((off, save)) => {
                    pos += off;
                    out.resize(pos + 4096, 0);
                    save.resume(cells(&mut out[pos..]))
                }
            };
        };
        start = loop {
            block = match block.step() {
                Complete(enc) => break enc,
                Incomplete((off, save)) => {
                    pos += off;
                    out.resize(pos + 4096, 0);
                    save.resume(cells(&mut out[pos..]))
                }
            };
        };
    }
    let off = start.flush();
    out.truncate(pos + off);
    out
}

fn decompress(bzz: &[u8], scratch: &mut Scratch) -> Result<Vec<u8>, super::dec::Error> {
    use super::dec::*;

    let mut out = vec![];
    let mut start = start(bzz);
    loop {
        let mut block = loop {
            start = match start.step(scratch) {
                Complete(None) => return Ok(out),
                Complete(Some(enc)) => break enc,
                Incomplete(save) => save.seal(),
            };
        };
        let (shuffle, next) = loop {
            block = match block.step()? {
                Complete((shuf, enc)) => break (shuf, enc),
                Incomplete(save) => save.seal(),
            };
        };
        let pos = out.len();
        out.resize(pos + shuffle.len(), 0);
        shuffle.run(cells(&mut out[pos..]));
        start = next;
    }
}

proptest! {
    #[test]
    fn bwt_round_trip(input in prop::collection::vec(prop::num::u8::ANY, 1..1_000)) {
        let mut scratch = Scratch::new();
        let marker = super::enc::bwt(&input, &mut scratch);
        let mut buf = vec![0; input.len()];
        super::dec::bwt_inv(marker, cells(&mut buf[..]), &mut scratch);
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
