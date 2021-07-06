// sndjvu-tree
// Copyright (C) 2021 Cole Miller
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

use bstr::{BStr, ByteSlice};
use sndjvu::codec::bzz;
use sndjvu::format::{self, ant, document, multi_page, page, txt};
use sndjvu::{ComponentMetaMismatchError, MissingComponentError};
use std::io::prelude::*;

const TREE_SHIFT: u32 = 4;
const TXT_EXCERPT_WORDS: u32 = 8;

#[derive(Debug)]
struct PrettySize(usize);

impl std::fmt::Display for PrettySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn rounding_div(n: usize, d: usize) -> usize {
            let rem = n % d;
            if 2 * rem >= d {
                n / d + 1
            } else {
                n / d
            }
        }

        const K: usize = 1000;
        let n = self.0;
        if n >= K * K * K {
            write!(f, "{: >3} G", rounding_div(n, K * K * K))
        } else if n >= K * K {
            write!(f, "{: >3} M", rounding_div(n, K * K))
        } else if n >= K {
            write!(f, "{: >3} k", rounding_div(n, K))
        } else {
            write!(f, "{: >3}  ", n)
        }
    }
}

struct TreeWriter<W> {
    depth: u32,
    inner: W,
}

impl<W: Write> TreeWriter<W> {
    fn new(inner: W) -> Self {
        Self { depth: 0, inner }
    }

    fn write_line(&mut self, args: std::fmt::Arguments) -> std::io::Result<()> {
        self.inner.write_fmt(format_args!(
            "{:width$}{}\n",
            "",
            args,
            width = self.depth as _
        ))?;
        Ok(())
    }

    fn write_line_and_shift(&mut self, args: std::fmt::Arguments) -> std::io::Result<()> {
        self.write_line(args)?;
        self.depth += TREE_SHIFT;
        Ok(())
    }

    fn unshift(&mut self) {
        self.depth -= TREE_SHIFT;
    }
}

fn handle_txt(
    txt: txt::Txt<'_>,
    sink: &mut TreeWriter<impl Write>,
    bzz_buf: &mut bzz::dec::Buffer,
    bzz_scratch: &mut bzz::Scratch,
) -> Result<(), Box<dyn std::error::Error>> {
    fn excerpt(s: &BStr) -> &BStr {
        let end = s
            .word_indices()
            .nth(TXT_EXCERPT_WORDS as usize - 1)
            .map(|(_, i, _)| i)
            .unwrap_or(s.len());
        &s[..end]
    }

    let (id, size, layout) = match txt {
        txt::Txt::Raw(raw) => ("TXTa", raw.bytes().len(), raw.parse()?),
        txt::Txt::Encoded(encoded) => (
            "TXTz",
            encoded.bytes().len(),
            encoded.decode_and_parse(bzz_buf, bzz_scratch)?,
        ),
    };
    sink.write_line(format_args!(
        "{} [{}] v{} {:?}",
        id,
        PrettySize(size),
        layout.version,
        excerpt(layout.text)
    ))?;
    Ok(())
}

fn handle_iw44(
    id: [u8; 4],
    iw44: page::Iw44<'_>,
    sink: &mut TreeWriter<impl Write>,
) -> Result<(), Box<dyn std::error::Error>> {
    let size = iw44.bytes().len();
    match iw44.parse()?.kind {
        page::Iw44Kind::Head {
            version,
            colors,
            width,
            height,
            ..
        } => {
            sink.write_line(format_args!(
                "{} [{}] v{} width={} height={} {}",
                id.as_bstr(),
                PrettySize(size),
                version,
                width,
                height,
                match colors {
                    page::Iw44ColorsKind::YCbCr => "color",
                    page::Iw44ColorsKind::Grayscale => "grayscale",
                }
            ))?;
        }
        page::Iw44Kind::Tail(_) => {
            sink.write_line(format_args!("{} [{}]", id.as_bstr(), PrettySize(size)))?
        }
    }
    Ok(())
}

fn handle_whatever(
    id: [u8; 4],
    size: usize,
    sink: &mut TreeWriter<impl Write>,
) -> Result<(), Box<dyn std::error::Error>> {
    sink.write_line(format_args!("{} [{}]", id.as_bstr(), PrettySize(size)))?;
    Ok(())
}

fn handle_elements(
    mut elements: page::PageElements<'_>,
    sink: &mut TreeWriter<impl Write>,
    bzz_buf: &mut bzz::dec::Buffer,
    bzz_scratch: &mut bzz::Scratch,
) -> Result<(), Box<dyn std::error::Error>> {
    while let Some(element) = elements.try_next()? {
        match element {
            page::Element::Ant(ant) => match ant {
                ant::Ant::Raw(_) => sink.write_line(format_args!("ANTa"))?,
                ant::Ant::Encoded(_) => sink.write_line(format_args!("ANTz"))?,
            },
            page::Element::Txt(txt) => handle_txt(txt, sink, bzz_buf, bzz_scratch)?,
            page::Element::Incl(incl) => {
                sink.write_line(format_args!("INCL {}", incl.target_id()))?
            }
            page::Element::Djbz(djbz) => {
                sink.write_line(format_args!("Djbz [{}]", PrettySize(djbz.bytes().len())))?
            }
            page::Element::Sjbz(sjbz) => {
                sink.write_line(format_args!("Sjbz [{}]", PrettySize(sjbz.bytes().len())))?
            }
            page::Element::Fgbz(fgbz) => sink.write_line(format_args!(
                "FGbz [{}] v{}",
                PrettySize(fgbz.bytes().len()),
                fgbz.parse()?.version
            ))?,
            page::Element::Fg44(iw44) => handle_iw44(*b"FG44", iw44, sink)?,
            page::Element::Bg44(iw44) => handle_iw44(*b"BG44", iw44, sink)?,
            page::Element::Fgjp(fgjp) => handle_whatever(*b"FGjp", fgjp.bytes().len(), sink)?,
            page::Element::Bgjp(bgjp) => handle_whatever(*b"BGjp", bgjp.bytes().len(), sink)?,
            page::Element::Smmr(smmr) => handle_whatever(*b"Smmr", smmr.bytes().len(), sink)?,
            page::Element::Unrecognized(chunk) => {
                handle_whatever(chunk.id, chunk.content_bytes().len(), sink)?
            }
        }
    }
    Ok(())
}

fn handle_page(
    component_meta: Option<multi_page::ComponentMeta<'_>>,
    page: Option<page::Page<'_>>,
    sink: &mut TreeWriter<impl Write>,
    bzz_buf: &mut bzz::dec::Buffer,
    bzz_scratch: &mut bzz::Scratch,
) -> Result<(), Box<dyn std::error::Error>> {
    match (component_meta, page) {
        (Some(multi_page::ComponentMeta { id, .. }), Some(page)) => {
            sink.write_line_and_shift(format_args!(
                "DJVU {} v{} width={} height={}",
                id, page.version, page.width, page.height
            ))?;
            handle_elements(page.elements(), sink, bzz_buf, bzz_scratch)?;
            sink.unshift();
        }
        (Some(multi_page::ComponentMeta { id, .. }), None) => {
            sink.write_line(format_args!("DJVU {}", id))?
        }
        (None, Some(page)) => {
            sink.write_line_and_shift(format_args!(
                "DJVU v{} width={} height={}",
                page.version, page.width, page.height
            ))?;
            handle_elements(page.elements(), sink, bzz_buf, bzz_scratch)?;
            sink.unshift();
        }
        (None, None) => panic!("tool bug"),
    }
    Ok(())
}

fn handle_bundle(
    component_meta: multi_page::ComponentMeta<'_>,
    elements: Option<page::PageElements<'_>>,
    sink: &mut TreeWriter<impl Write>,
    bzz_buf: &mut bzz::dec::Buffer,
    bzz_scratch: &mut bzz::Scratch,
) -> Result<(), Box<dyn std::error::Error>> {
    sink.write_line_and_shift(format_args!("DJVI {}", component_meta.id))?;
    if let Some(elements) = elements {
        handle_elements(elements, sink, bzz_buf, bzz_scratch)?;
    }
    sink.unshift();
    Ok(())
}

fn handle_thumbnails(
    component_meta: multi_page::ComponentMeta<'_>,
    thumbnails: Option<multi_page::Thumbnails<'_>>,
    sink: &mut TreeWriter<impl Write>,
) -> Result<(), Box<dyn std::error::Error>> {
    sink.write_line_and_shift(format_args!("THUM {}", component_meta.id))?;
    if let Some(mut thumbnails) = thumbnails {
        while let Some(th44) = thumbnails.try_next()? {
            handle_iw44(*b"TH44", th44, sink)?;
        }
    }
    sink.unshift();
    Ok(())
}

fn handle_multi_page(
    multi_page: multi_page::MultiPage<'_>,
    sink: &mut TreeWriter<impl Write>,
    bzz_buf: &mut bzz::dec::Buffer,
    bzz_scratch: &mut bzz::Scratch,
) -> Result<(), Box<dyn std::error::Error>> {
    sink.write_line_and_shift(format_args!(
        "DJVM v{} {}",
        multi_page.version,
        if multi_page.bundled.is_some() {
            "bundled"
        } else {
            "indirect"
        }
    ))?;
    let mut meta_buf = bzz::dec::Buffer::new();
    let meta = multi_page
        .meta
        .decode_and_parse(&mut meta_buf, bzz_scratch)?
        .into_iter();
    if let Some(bundled) = multi_page.bundled {
        let mut bundled_components = bundled.components();
        for component_meta in meta {
            let component = bundled_components.try_next()?.ok_or_else(|| {
                Box::new(MissingComponentError {
                    expected: component_meta.kind,
                })
            })?;
            if component.kind() != component_meta.kind {
                return Err(Box::new(ComponentMetaMismatchError {
                    expected: component_meta.kind,
                    found: component.kind(),
                }));
            }
            match component {
                multi_page::Component::Bundle(elements) => {
                    handle_bundle(component_meta, Some(elements), sink, bzz_buf, bzz_scratch)?
                }
                multi_page::Component::Page(page) => {
                    handle_page(Some(component_meta), Some(page), sink, bzz_buf, bzz_scratch)?
                }
                multi_page::Component::Thumbnails(thumbnails) => {
                    handle_thumbnails(component_meta, Some(thumbnails), sink)?
                }
            }
        }
    } else {
        for component_meta in meta {
            match component_meta.kind {
                multi_page::ComponentKind::Bundle => {
                    handle_bundle(component_meta, None, sink, bzz_buf, bzz_scratch)?
                }
                multi_page::ComponentKind::Page => {
                    handle_page(Some(component_meta), None, sink, bzz_buf, bzz_scratch)?
                }
                multi_page::ComponentKind::Thumbnails => {
                    handle_thumbnails(component_meta, None, sink)?
                }
            }
        }
    }
    sink.unshift();
    Ok(())
}

fn main_inner() -> Result<(), Box<dyn std::error::Error>> {
    let mut bzz_buf = bzz::dec::Buffer::new();
    let mut bzz_scratch = bzz::Scratch::new();
    let stdout = std::io::stdout();
    let mut sink = TreeWriter::new(stdout.lock());
    let mut doc_buf = Vec::new();
    std::io::stdin().lock().read_to_end(&mut doc_buf)?;
    let document = format::parse_document(&doc_buf)?;
    match document {
        document::Document::SinglePage(page) => {
            handle_page(None, Some(page), &mut sink, &mut bzz_buf, &mut bzz_scratch)?
        }
        document::Document::MultiPage(multi_page) => {
            handle_multi_page(multi_page, &mut sink, &mut bzz_buf, &mut bzz_scratch)?
        }
    }
    Ok(())
}

fn main() {
    if let Err(e) = main_inner() {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}
