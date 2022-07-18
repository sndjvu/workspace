use super::Scratch;
use proptest::prelude::*;

proptest! {
    #[test]
    fn bwt_round_trip(input in prop::collection::vec(prop::num::u8::ANY, 1..1_000)) {
        use std::{vec, format};

        let mut scratch = Scratch::new();
        let marker = super::enc::bwt(&input, &mut scratch);
        let mut buf = vec![0; input.len()];
        super::dec::bwt_inv(marker, &mut buf, &mut scratch);
        prop_assert_eq!(input, buf);
    }
}

