use crate::zp;
use super::{Arena, Contexts, decode_integer};
use alloc::vec;
use alloc::vec::Vec;
use core::num::NonZeroUsize;
use alloc::boxed::Box;

pub struct Error {
}

struct Symbol {
    width: usize,
    height: usize,
    data: Vec<u8>,
}

pub struct Library {
    depth: usize,
    symbols: Vec<Symbol>,
}

impl Library {
    fn len(&self) -> usize {
        self.symbols.len()
    }

}

pub trait Canvas {
    fn depth(&self) -> NonZeroUsize;
    fn clear_pixels(&self, data: &mut [u8]);
    fn set_pixels(&self, data: &mut [u8]);

    fn start(&mut self, width: usize, height: usize);
    fn row(&mut self, i: usize, j: usize, data: &[u8]);
    fn comment(&mut self, _comment: &[u8]) {}
}

pub struct Decoder<'a> {
    arena: Arena,
    baselines: [i32; 3],
    bitmap: Vec<u8>,
    canvas_set_pixel: Option<Box<[u8]>>,
    comment: Vec<u8>,
    contexts: Contexts,
    first_on_line_bottom_row: i32,
    first_on_line_left_column: i32,
    image_height: i32,
    image_width: i32,
    library: Library,
    prev_on_line_right_column: i32,
    start: bool,
    stop: bool,
    symbol_count: u32,
    zp: zp::Decoder<'a>,
}

fn make_index(bitmap: &[u8], width: usize, i: usize, j: usize, offs: &[(i32, i32)], set_pixel: &[u8]) -> usize {
    let depth = set_pixel.len();
    let mut index = 0;
    for &(di, dj) in offs {
        index <<= 1;
        let start = width * (i + di) + (j + dj);
        if bitmap[start * depth..(start + 1) * depth].eq(set_pixel) {
            index |= 1;
        }
    }
    index
}

/// Convert a a coordinate pair in the JB2 system (1-based, origin at lower left)
/// to the "normal person" system (0-based, origin at upper left).
fn cvt_coords(i: i32, j: i32, image_width: i32, image_height: i32) -> (usize, usize) {
    (image_height - i, j - 1)
}

fn estimate_baseline(baselines: &[i32; 3], count: u32) -> i32 {
    use std::cmp::Ordering::*;

    if count >= 3 {
        let [x, y, z] = *baselines;
        match x.cmp(&y) {
            Less => match x.cmp(&z) {
                Less => y.min(z),
                Equal => y,
                Greater => x,
            },
            Equal => x,
            Greater => match x.cmp(&z) {
                Less => x,
                Equal => z,
                Greater => z.max(y),
            },
        }
    } else {
        baselines[count - 1]
    }
}

impl<'a> Decoder<'a> {
    pub fn record<C: Canvas>(&mut self, canvas: &mut C) -> Result<Option<()>, Error> {
        let Self {
            ref mut arena,
            ref mut bitmap,
            ref mut canvas_set_pixel,
            ref mut comment,
            ref mut contexts,
            ref mut first_on_line_column,
            ref mut first_on_line_row,
            ref mut image_height,
            ref mut image_width,
            ref mut library,
            ref mut start,
            ref mut stop,
            ref mut symbol_count,
            ref mut zp,
            ref mut baselines,
            ref mut first_on_line_bottom_row,
            ref mut first_on_line_left_column,
        } = *self;
        if *stop {
            return Ok(None);
        }

        let canvas_set_pixel = canvas_set_pixel.get_or_insert_with(|| {
            let depth = canvas.depth();
            assert!(depth == library.depth());
            let mut pixel = vec![0x00; depth].into_boxed_slice();
            canvas.set_pixels(&mut pixel[..]);
            pixel
        });
        let canvas_depth = canvas_set_pixel.len();

        let record_type = decode_integer(zp, &mut contexts.record_type, arena);
        let got = match record_type {
            0 => {
                if *start {
                    return Err(Error {});
                }
                let width = decode_integer(zp, &mut contexts.image_size, arena);
                let height = decode_integer(zp, &mut contexts.image_size, arena);
                let _eventual_image_refinement = zp.decode(&mut contexts.eventual_image_refinement);
                *image_width = width;
                *image_height = height;
                contexts.symbol_column_number.hi = width;
                contexts.symbol_row_number.hi = height;
                canvas.start(width.try_into().unwrap(), height.try_into().unwrap());
                Some(())
            }
            1 => {
                if !*start {
                    return Err(Error {});
                }

                let width: usize = decode_integer(zp, &mut contexts.symbol_width, arena).try_into().unwrap();
                let height: usize = decode_integer(zp, &mut contexts.symbol_height, arena).try_into().unwrap();
                // The width and height that we'll use while decoding the bitmap;
                // these are padded so that we can handle everything uniformly in
                // make_index.
                let full_width = 2 + width + 2;
                let full_height = 2 + height;
                bitmap.resize(full_width * full_height * canvas_depth, 0xff);
                canvas.clear_pixels(&mut bitmap[..]);
                for i in 0..height {
                    for j in 0..width {
                        let x = make_index(
                            &bitmap[..], full_width,
                            2 + i, 2 + j,
                            &[
                                          (-2, -1), (-2, -0), (-2, 1),
                                (-1, -2), (-1, -1), (-1, -0), (-1, 1), (-1, 2),
                                (-0, -2), (-0, -1),
                            ],
                            &canvas_set_pixel[..],
                        );
                        if zp.decode(&mut contexts.direct_bitmap[x]) {
                            let start = full_width * (2 + i) + (2 + j);
                            canvas.set_pixels(&mut bitmap[start * canvas_depth..(start + 1) * canvas_depth]);
                        }
                    }
                }

                let (column, row) = if zp.decode(&mut contexts.offset_type) {
                    let column_offset = decode_integer(zp, &mut contexts.new_line_column_offset, arena);
                    let row_offset = decode_integer(zp, &mut contexts.new_line_row_offset, arena);
                    let column = column_offset + *first_on_line_left_column;
                    let row = row_offset + *first_on_line_bottom_row;
                    *first_on_line_left_column = column;
                    *first_on_line_bottom_row = row - height;
                    *prev_on_line_right_column = column + width;
                    baselines.fill(0);
                    baselines[0] = row;
                    *symbol_count = 1;
                    cvt_coords(column, row, *image_width, *image_height)
                } else {
                    let column_offset = decode_integer(zp, &mut contexts.same_line_column_offset, arena);
                    let row_offset = decode_integer(zp, &mut contexts.same_line_row_offset, arena);
                    let column = column_offset + *prev_on_line_right_column;
                    let row = row_offset + estimate_baseline(baselines, *symbol_count) + height;
                    *prev_on_line_right_column = column + width;
                    baselines.rotate_right(1);
                    baselines[0] = row;
                    *symbol_count += 1;
                    cvt_coords(column, row, *image_width, *image_height)
                };

                for i in 0..height {
                    let start = full_width * (2 + i) + 2;
                    canvas.row(row + i, column, &bitmap[start * depth..(start + width) * depth]);
                }
                library.trim_and_add(take(bitmap), full_width, full_height);
                Some(())
            }
            2 => todo!(),
            3 => todo!(),
            4 => todo!(),
            5 => todo!(),
            6 => todo!(),
            7 => todo!(),
            8 => todo!(),
            9 => {
                if !*start {
                    return Err(Error {});
                }
                let length = decode_integer(zp, &mut contexts.comment_length, arena).try_into().unwrap();
                comment.clear();
                comment.reserve(length);
                for _ in 0..length {
                    let octet = decode_integer(zp, &mut contexts.comment_octet, arena).try_into().unwrap();
                    comment.push(octet);
                }
                canvas.comment(&comment[..]);
                Some(())
            }
            10 => {
                if *start {
                    contexts.reset_numcoder();
                } else {
                    let expected = decode_integer(zp, &mut contexts.dictionary_size, arena).try_into().unwrap();
                    if self.library.len() != expected {
                        return Err(Error {});
                    }
                }
                Some(())
            }
            11 => {
                if !*start {
                    return Err(Error {})
                }
                *stop = true;
                None
            }
            _ => unreachable!(),
        };
        Ok(got)
    }

    pub fn into_library(self) -> Library {
        self.library
    }
}
