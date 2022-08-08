#![no_std]
#![cfg_attr(sndjvu_doc_cfg, feature(doc_cfg, doc_auto_cfg))]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

pub extern crate sndjvu_format as format;
pub extern crate sndjvu_codec as codec;
