use proptest::prelude::*;

use super::{Context, enc, Encoder, dec, Decoder};
use crate::Update;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Record {
    context: Option<usize>,
    decision: bool,
}

prop_compose! {
    fn record(num_contexts: usize)
        (context in prop::option::of(0..num_contexts), decision in prop::bool::ANY)
            -> Record {
        Record { context, decision }
    }
}

proptest! {
    #[test]
    fn round_trip(records in prop::collection::vec(record(100), 1..1_000)) {
        use std::{vec, format};

        let mut contexts = [Context::NEW; 100];
        let mut buf = vec![0xff; 8 * records.len()];
        let mut encoder = match Encoder::new(&mut buf[..]).provision(records.len() as u32) {
            Update::Success(enc) => enc,
            Update::Suspend(_) => panic!(),
        };
        for &Record { context, decision } in &records {
            match context {
                None => encoder.encode_passthrough(decision),
                Some(i) => encoder.encode(decision, &mut contexts[i]),
            }
        }
        let _end = encoder.flush();
        let mut decoder = match Decoder::new(&buf[..]).provision(records.len() as u32) {
            Update::Success(dec) => dec,
            Update::Suspend(_) => panic!(),
        };
        for &Record { context, decision } in &records {
            match context {
                None => prop_assert_eq!(decoder.decode_passthrough(), decision),
                Some(i) => prop_assert_eq!(decoder.decode(&mut contexts[i]), decision)
            }
        }
    }
}
