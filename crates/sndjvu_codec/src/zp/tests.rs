use proptest::prelude::*;

use super::{Context, Encoder, Decoder};
use crate::{Step::*, cells};

const NUM_CONTEXTS: usize = 3;

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
    fn round_trip(records in prop::collection::vec(record(NUM_CONTEXTS), 1..1_000)) {
        use std::{vec, format};

        let mut contexts = [Context::NEW; NUM_CONTEXTS];
        let mut buf = vec![0xff; 8 * records.len()]; // XXX
        let mut encoder = match Encoder::new(cells(&mut buf[..])).provision(records.len() as u32) {
            Complete(enc) => enc,
            Incomplete(_) => panic!(),
        };
        for &Record { context, decision } in &records {
            match context {
                None => encoder.encode_passthrough(decision),
                Some(i) => encoder.encode(decision, &mut contexts[i]),
            }
        }
        let end = encoder.flush();
        contexts = [Context::NEW; NUM_CONTEXTS];
        let mut decoder = match Decoder::new(&buf[..end]).provision(records.len() as u32) {
            Complete(dec) => dec,
            Incomplete(save) => save.seal_provision(records.len() as u32),
        };
        for &Record { context, decision } in &records {
            match context {
                None => prop_assert_eq!(decoder.decode_passthrough(), decision),
                Some(i) => prop_assert_eq!(decoder.decode(&mut contexts[i]), decision)
            }
        }
    }
}
