use core::ops::RangeInclusive;
use core::num::NonZeroU16;
use alloc::boxed::Box;
use alloc::vec::Vec;
use crate::zp;
use alloc::vec;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
struct Ref(NonZeroU16);

struct Node {
    context: zp::Context,
    left: Option<Ref>,
    right: Option<Ref>,
}

struct Arena {
    nodes: Vec<Node>,
}

impl Arena {
    fn get(&mut self, it: Ref) -> &mut Node {
        todo!()
    }

    fn push(&mut self) -> Ref {
        todo!()
    }
}

struct IntegerContext {
    lo: i32,
    hi: i32,
    root: Option<Ref>,
}

impl IntegerContext {
    fn new(range: RangeInclusive<i32>) -> Self {
        todo!()
    }
}

fn decode_integer(zp: &mut zp::Decoder<'_>, context: &mut IntegerContext, arena: &mut Arena) -> i32 {
    let mut is_negative = false;
    let mut cutoff = 0;
    let mut x = context.root.unwrap_or_else(|| arena.push());
    let mut lo = context.lo;
    let mut hi = context.hi;
    let mut phase = 1;
    let mut range = -1;

    while range != 1 {
        let mut node = arena.get(x);
        let decision = lo >= cutoff || (hi >= cutoff && zp.decode(&mut node.context));
        x = if decision {
            match node.right {
                None => {
                    let y = arena.push();
                    let mut node = arena.get(x);
                    node.right = Some(y);
                    y
                }
                Some(y) => y,
            }
        } else {
            match node.left {
                None => {
                    let y = arena.push();
                    let mut node = arena.get(x);
                    node.left = Some(y);
                    y
                }
                Some(y) => y,
            }
        };

        match phase {
            1 => {
                is_negative = !decision;
                if is_negative {
                    let temp = -lo - 1;
                    lo = -hi - 1;
                    hi = temp;
                }
                phase = 2;
                cutoff = 1;
            }
            2 => {
                if !decision {
                    phase = 3;
                    range = (cutoff + 1) / 2;
                    cutoff = if range == 1 { 0 } else { cutoff - range / 2 };
                } else {
                    cutoff += cutoff + 1;
                }
            }
            3 => {
                range /= 2;
                if range != 1 {
                    if !decision {
                        cutoff -= range / 2;
                    } else {
                        cutoff += range / 2;
                    }
                } else if !decision {
                    cutoff -= 1;
                }
            }
            _ => unreachable!(),
        }
    }

    if is_negative {
        -cutoff - 1
    } else {
        cutoff
    }
}

struct Contexts {
    record_type: IntegerContext,
    image_size: IntegerContext,
    matching_symbol_index: IntegerContext,
    symbol_width: IntegerContext,
    symbol_height: IntegerContext,
    symbol_width_difference: IntegerContext,
    symbol_height_difference: IntegerContext,
    symbol_column_number: IntegerContext,
    symbol_row_number: IntegerContext,
    same_line_column_offset: IntegerContext,
    same_line_row_offset: IntegerContext,
    new_line_column_offset: IntegerContext,
    new_line_row_offset: IntegerContext,
    comment_length: IntegerContext,
    comment_octet: IntegerContext,
    dictionary_size: IntegerContext,

    direct_bitmap: Box<[zp::Context; 1024]>,
    refinement_bitmap: Box<[zp::Context; 2048]>,

    eventual_image_refinement: zp::Context,
    offset_type: zp::Context,
}

impl Contexts {
    fn new() -> Self {
        const HI: i32 = 262142;
        const LO: i32 = -262143;

        Self {
            record_type: IntegerContext::new(0..=11),
            image_size: IntegerContext::new(0..=HI),
            matching_symbol_index: IntegerContext::new(0..=-1),
            symbol_width: IntegerContext::new(0..=HI),
            symbol_height: IntegerContext::new(0..=HI),
            symbol_width_difference: IntegerContext::new(LO..=HI),
            symbol_height_difference: IntegerContext::new(LO..=HI),
            symbol_column_number: IntegerContext::new(1..=0),
            symbol_row_number: IntegerContext::new(1..=0),
            same_line_column_offset: IntegerContext::new(LO..=HI),
            same_line_row_offset: IntegerContext::new(LO..=HI),
            new_line_column_offset: IntegerContext::new(LO..=HI),
            new_line_row_offset: IntegerContext::new(LO..=HI),
            comment_length: IntegerContext::new(0..=HI),
            comment_octet: IntegerContext::new(0..=255),
            dictionary_size: IntegerContext::new(0..=HI),

            direct_bitmap: vec![zp::Context::NEW; 1024].into_boxed_slice().try_into().unwrap(),
            refinement_bitmap: vec![zp::Context::NEW; 2048].into_boxed_slice().try_into().unwrap(),

            eventual_image_refinement: zp::Context::NEW,
            offset_type: zp::Context::NEW,
        }
    }

    fn reset_numcoder(&mut self) {
        todo!()
    }
}

pub mod enc;
pub mod dec;
