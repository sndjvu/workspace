use super::{Scratch, Split};
use proptest::prelude::*;
use std::{vec, format};

proptest! {
    #[test]
    fn bwt_round_trip(input in prop::collection::vec(prop::num::u8::ANY, 1..1_000)) {
        let mut scratch = Scratch::new();
        let marker = super::enc::bwt(&input, &mut scratch);
        let mut buf = vec![0; input.len()];
        super::dec::bwt_inv(marker, &mut buf, &mut scratch);
        prop_assert_eq!(input, buf);
    }

    #[test]
    fn round_trip(input in prop::collection::vec(prop::num::u8::ANY, 10..1_000)) {
        let compressed = super::compress_oneshot(&input, Split::Default);
        let output = super::decompress_oneshot(&compressed).unwrap();
        prop_assert_eq!(output, input);
    }
}
