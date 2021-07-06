// sndjvu_format::ant
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

use crate::chunk::Leaf;
use crate::{Bzz, BzzBuffer};
use bstr::{BStr, BString, ByteSlice};
use rgb::RGB8;

#[derive(Clone, Debug)]
struct Parsing<'a> {
    location: crate::ParseLocation,
    inner: &'a BStr,
}

impl<'a> Into<Parsing<'a>> for crate::Parsing<'a> {
    fn into(self) -> Parsing<'a> {
        let crate::Parsing { location, inner } = self;
        Parsing {
            location,
            inner: inner.as_bstr(),
        }
    }
}

/// An `ANTa` chunk.
#[derive(Clone, Debug)]
pub struct RawAnt<'raw> {
    content: Parsing<'raw>,
}

impl<'raw> RawAnt<'raw> {
    pub fn bytes(&self) -> &'raw [u8] {
        self.content.inner
    }

    pub fn parse(&self) -> Annotations<'raw> {
        Annotations {
            parsing: self.content.clone(),
        }
    }
}

/// An `ANTz` chunk.
#[derive(Clone, Debug)]
pub struct EncodedAnt<'raw> {
    bzz: Bzz<'raw>,
}

impl<'raw> EncodedAnt<'raw> {
    pub fn decode_and_parse<'dec, B: BzzBuffer>(
        &self,
        buffer: &'dec mut B,
        scratch: &mut B::Scratch,
    ) -> Annotations<'dec> {
        let parsing = self.bzz.parsing_decoded(buffer, scratch).into();
        Annotations { parsing }
    }
}

#[derive(Clone, Debug)]
pub enum Ant<'raw> {
    Raw(RawAnt<'raw>),
    Encoded(EncodedAnt<'raw>),
}

impl<'raw> Ant<'raw> {
    pub(crate) fn from_anta(chunk: Leaf<'raw>) -> Self {
        Self::Raw(RawAnt {
            content: chunk.content.into(),
        })
    }

    pub(crate) fn from_antz(chunk: Leaf<'raw>) -> Self {
        Self::Encoded(EncodedAnt {
            bzz: chunk.content.bzz(),
        })
    }
}

#[derive(Clone, Debug)]
pub enum Annotation<'a> {
    Background(RGB8),
    Zoom(Zoom),
    Mode(Mode),
    Align { horiz: HorizAlign, vert: VertAlign },
    MapArea(MapArea<'a>),
    Header(MarginStrings<'a>),
    Footer(MarginStrings<'a>),
    // TODO metadata annotations
}

#[derive(Clone, Debug)]
pub enum AnnotationError {
    BadMarginString(BString),
    BadWord(String),
    EndOfInput,
    InappropriateEffect(&'static str),
    MissingNumber,
    MissingWord,
    MissingWhitespace,
    UnclosedStr,
    Unexpected(BString),
    UnmatchedParen,
}

impl AnnotationError {
    fn unexpected(found: &[u8]) -> Self {
        Self::Unexpected(found.as_bstr().to_owned())
    }

    fn bad_word(found: &[u8]) -> Self {
        Self::BadWord(found.as_bstr().to_string())
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct EscapedStr(BStr);

impl EscapedStr {
    fn from_ref(raw: &BStr) -> &Self {
        // SAFETY EscapedStr is repr(transparent)
        unsafe { std::mem::transmute(raw) }
    }

    pub fn raw(&self) -> &BStr {
        &self.0
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Zoom {
    Stretch,
    OneToOne,
    Width,
    Page,
    D(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Color,
    Bw,
    Fore,
    Black,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HorizAlign {
    Left,
    Center,
    Right,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VertAlign {
    Top,
    Center,
    Bottom,
}

#[derive(Clone, Debug)]
pub enum Shape {
    Rect {
        origin: Point,
        width: u32,
        height: u32,
        highlight: Option<Highlight>,
    },
    Oval {
        origin: Point,
        width: u32,
        height: u32,
    },
    Text {
        origin: Point,
        width: u32,
        height: u32,
        background_color: Option<RGB8>,
        text_color: RGB8,
        collapsible: bool,
    },
    Poly {
        vertices: Vec<Point>,
    },
    Line {
        endpoints: [Point; 2],
        arrow: bool,
        width: u32,
        color: RGB8,
    },
}

#[derive(Debug)]
struct XRgb(RGB8);

impl std::fmt::Display for XRgb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let RGB8 { r, g, b } = self.0;
        write!(f, "#{:02X}{:02X}{:02X}", r, g, b)
    }
}

impl std::fmt::Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::Rect {
                origin: Point { x, y },
                width,
                height,
                highlight,
            } => {
                write!(f, "(rect {} {} {} {})", x, y, width, height)?;
                if let Some(Highlight { color, opacity }) = highlight {
                    write!(f, " (hilite {}) (opacity {})", XRgb(color), opacity)?;
                }
                Ok(())
            }
            Self::Oval {
                origin: Point { x, y },
                width,
                height,
            } => {
                write!(f, "(oval {} {} {} {})", x, y, width, height)?;
                Ok(())
            }
            Self::Text {
                origin: Point { x, y },
                width,
                height,
                background_color,
                text_color,
                collapsible,
            } => {
                write!(f, "(text {} {} {} {})", x, y, width, height)?;
                if let Some(color) = background_color {
                    write!(f, " (backclr {})", XRgb(color))?;
                }
                write!(f, " (textclr {})", XRgb(text_color))?;
                if collapsible {
                    write!(f, " (pushpin)")?;
                }
                Ok(())
            }
            Self::Poly { ref vertices } => {
                write!(f, "(poly")?;
                for Point { x, y } in vertices {
                    write!(f, " {} {}", x, y)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Self::Line {
                endpoints,
                arrow,
                width,
                color,
            } => {
                write!(
                    f,
                    "(line {} {} {} {})",
                    endpoints[0].x, endpoints[0].y, endpoints[1].x, endpoints[1].y
                )?;
                if arrow {
                    write!(f, " (arrow)")?;
                }
                write!(f, " (width {}) (lineclr {})", width, XRgb(color))?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct MapArea<'a> {
    pub link: Link<'a>,
    pub comment: &'a EscapedStr,
    pub shape: Shape,
    pub border: Border,
}

#[derive(Clone, Debug)]
pub struct Link<'a> {
    dest: LinkDest<'a>,
    target: Option<&'a EscapedStr>,
}

#[derive(Clone, Debug)]
pub enum LinkDest<'a> {
    External(&'a EscapedStr),
    Internal(&'a EscapedStr),
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Point {
    pub x: u32,
    pub y: u32,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Highlight {
    pub color: RGB8,
    pub opacity: u32,
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq)]
pub struct Border {
    pub kind: BorderKind,
    pub always_visible: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BorderKind {
    None,
    Xor,
    Color(RGB8),
    ShadowIn(u32),
    ShadowOut(u32),
}

impl Default for BorderKind {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Clone, Debug, Default)]
pub struct MarginStrings<'a> {
    pub left: Option<&'a EscapedStr>,
    pub center: Option<&'a EscapedStr>,
    pub right: Option<&'a EscapedStr>,
}

pub struct Annotations<'a> {
    parsing: Parsing<'a>,
}

impl<'a> Annotations<'a> {
    pub fn try_next(&mut self) -> Result<Option<Annotation<'a>>, AnnotationError> {
        self.parsing.annotation()
    }
}

#[cfg(feature = "impl-fallible-iterator")]
impl<'a> fallible_iterator::FallibleIterator for Annotations<'a> {
    type Item = Annotation<'a>;
    type Error = AnnotationError;

    fn next(&mut self) -> Result<Option<Self::Item>, Self::Error> {
        self.try_next()
    }
}

#[derive(Debug)]
enum Opening<'a> {
    Word(&'a [u8]),
    CloseParen,
    EndOfInput,
}

impl<'a> Parsing<'a> {
    fn byte(&mut self) -> Result<u8, AnnotationError> {
        let (&first, rest) = self
            .inner
            .split_first()
            .ok_or(AnnotationError::EndOfInput)?;
        self.location.advance(1);
        self.inner = rest.as_bstr();
        Ok(first)
    }

    fn expect(&mut self, s: &'static str) -> Result<(), AnnotationError> {
        let rest = self
            .inner
            .strip_prefix(s.as_bytes())
            .ok_or_else(|| AnnotationError::unexpected(&self.inner[..s.len()]))?;
        self.location.advance(s.len());
        self.inner = rest.as_bstr();
        Ok(())
    }

    fn open(&mut self) -> Result<Opening<'a>, AnnotationError> {
        self.whitespace();
        match self.peek(1) {
            b"(" => {
                self.advance(1);
                self.whitespace();
                let word = self.word()?;
                Ok(Opening::Word(word))
            }
            b")" => Ok(Opening::CloseParen),
            b"" => Ok(Opening::EndOfInput),
            other => Err(AnnotationError::unexpected(other)),
        }
    }

    fn whitespace(&mut self) -> Option<()> {
        const WHITESPACE: &[u8] = b" \t\r\n";

        let end = self
            .inner
            .find_not_byteset(WHITESPACE)
            .unwrap_or_else(|| self.inner.len());
        if end == 0 {
            return None;
        }
        self.advance(end);
        Some(())
    }

    fn proper_whitespace(&mut self) -> Result<(), AnnotationError> {
        self.whitespace().ok_or(AnnotationError::MissingWhitespace)
    }

    fn close(&mut self) -> Result<(), AnnotationError> {
        self.whitespace();
        self.expect(")")?;
        Ok(())
    }

    fn color(&mut self) -> Result<RGB8, AnnotationError> {
        self.expect("#")?;
        let mut arr = [0; 3];
        for chan in &mut arr {
            for _ in 0..2 {
                *chan <<= 4;
                *chan |= match self.byte()? {
                    x @ b'0'..=b'9' => x - b'0',
                    x @ b'A'..=b'F' => 10 + x - b'A',
                    x @ b'a'..=b'f' => 10 + x - b'a',
                    other => return Err(AnnotationError::unexpected(&[other])),
                }
            }
        }
        Ok(arr.into())
    }

    fn advance(&mut self, by: usize) -> &'a [u8] {
        let (head, tail) = self.inner.split_at(by);
        self.location.advance(by);
        self.inner = tail.as_bstr();
        head
    }

    fn word(&mut self) -> Result<&'a [u8], AnnotationError> {
        const WORDWORTHY: &[u8] =
            b"_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        let end = self
            .inner
            .find_not_byteset(WORDWORTHY)
            .unwrap_or_else(|| self.inner.len());
        if end == 0 {
            return Err(AnnotationError::MissingWord);
        }
        Ok(self.advance(end))
    }

    fn zoom(&mut self) -> Result<Zoom, AnnotationError> {
        match self.word()? {
            b"stretch" => Ok(Zoom::Stretch),
            b"one2one" => Ok(Zoom::OneToOne),
            b"width" => Ok(Zoom::Width),
            b"page" => Ok(Zoom::Page),
            other => Err(AnnotationError::bad_word(other)),
        }
    }

    fn mode(&mut self) -> Result<Mode, AnnotationError> {
        match self.word()? {
            b"color" => Ok(Mode::Color),
            b"bw" => Ok(Mode::Bw),
            b"fore" => Ok(Mode::Fore),
            b"black" => Ok(Mode::Black),
            other => Err(AnnotationError::bad_word(other)),
        }
    }

    fn horiz_align(&mut self) -> Result<HorizAlign, AnnotationError> {
        match self.word()? {
            b"left" => Ok(HorizAlign::Left),
            b"center" => Ok(HorizAlign::Center),
            b"right" => Ok(HorizAlign::Right),
            other => Err(AnnotationError::bad_word(other)),
        }
    }

    fn vert_align(&mut self) -> Result<VertAlign, AnnotationError> {
        match self.word()? {
            b"top" => Ok(VertAlign::Top),
            b"center" => Ok(VertAlign::Center),
            b"bottom" => Ok(VertAlign::Bottom),
            other => Err(AnnotationError::bad_word(other)),
        }
    }

    fn margin_string(
        &mut self,
        target: &mut MarginStrings<'a>,
    ) -> Result<Option<()>, AnnotationError> {
        let full = match self.peek(1) {
            b"\"" => self.escaped_str()?.raw(),
            b")" => return Ok(None),
            b"" => return Err(AnnotationError::EndOfInput),
            other => return Err(AnnotationError::unexpected(other)),
        };
        if let Some(rest) = full.strip_prefix(b"left::") {
            target.left = Some(EscapedStr::from_ref(rest.as_bstr()));
            Ok(Some(()))
        } else if let Some(rest) = full.strip_prefix(b"center::") {
            target.center = Some(EscapedStr::from_ref(rest.as_bstr()));
            Ok(Some(()))
        } else if let Some(rest) = full.strip_prefix(b"right::") {
            target.right = Some(EscapedStr::from_ref(rest.as_bstr()));
            Ok(Some(()))
        } else {
            Err(AnnotationError::BadMarginString(full.as_bstr().to_owned()))
        }
    }

    fn peek(&self, n: usize) -> &'a [u8] {
        self.inner.get(..n).unwrap_or(b"")
    }

    fn number(&mut self) -> Result<u32, AnnotationError> {
        let end = self
            .inner
            .find_not_byteset(b"0123456789")
            .unwrap_or_else(|| self.inner.len());
        if end == 0 {
            return Err(AnnotationError::MissingNumber);
        }
        let raw = self.advance(end);
        let n: u32 = raw.iter().fold(0, |n, &b| n * 10 + (b - b'0') as u32);
        Ok(n)
    }

    fn point(&mut self) -> Result<Point, AnnotationError> {
        let x = self.number()?;
        self.proper_whitespace()?;
        let y = self.number()?;
        Ok(Point { x, y })
    }

    fn link_dest(&mut self) -> Result<LinkDest<'a>, AnnotationError> {
        match self.peek(1) {
            b"\"" => {
                let url = self.escaped_str()?.raw();
                if let Some((b'#', rest)) = url.split_first() {
                    Ok(LinkDest::Internal(EscapedStr::from_ref(rest.as_bstr())))
                } else {
                    Ok(LinkDest::External(EscapedStr::from_ref(url)))
                }
            }
            b"" => Err(AnnotationError::EndOfInput),
            other => Err(AnnotationError::unexpected(other)),
        }
    }

    fn link(&mut self) -> Result<Link<'a>, AnnotationError> {
        match self.peek(1) {
            b"\"" => {
                let dest = self.link_dest()?;
                Ok(Link { dest, target: None })
            }
            b"(" => {
                match self.open()? {
                    Opening::Word(b"url") => {}
                    Opening::Word(other) => return Err(AnnotationError::bad_word(other)),
                    _ => unreachable!(),
                }
                self.proper_whitespace()?;
                let dest = self.link_dest()?;
                self.proper_whitespace()?;
                let target = self.escaped_str()?;
                self.close()?;
                Ok(Link {
                    dest,
                    target: Some(target),
                })
            }
            b"" => Err(AnnotationError::EndOfInput),
            other => Err(AnnotationError::unexpected(other)),
        }
    }

    fn escaped_str(&mut self) -> Result<&'a EscapedStr, AnnotationError> {
        self.expect("\"")?;
        // the "real" closing quote is the first one that has an even number of backslashes
        // immediately preceding
        let end = self.inner.find_iter(b"\"").find(|&i| {
            let head = &self.inner[..i];
            let j = head.rfind_not_byteset(b"\\");
            let n = j.map_or(i, |j| i - j - 1);
            n % 2 == 0
        });
        end.map(|i| {
            let s = self.advance(i);
            // we know the closing quote is there because we found it earlier
            self.expect("\"").unwrap();
            EscapedStr::from_ref(s.as_bstr())
        })
        .ok_or(AnnotationError::UnclosedStr)
    }

    fn shape(&mut self) -> Result<Shape, AnnotationError> {
        const BLACK: RGB8 = RGB8 {
            r: 0x00,
            g: 0x00,
            b: 0x00,
        };

        match self.open()? {
            Opening::EndOfInput => Err(AnnotationError::EndOfInput),
            Opening::CloseParen => Err(AnnotationError::unexpected(b")")),
            Opening::Word(b"rect") => {
                self.proper_whitespace()?;
                let origin = self.point()?;
                self.proper_whitespace()?;
                let width = self.number()?;
                self.proper_whitespace()?;
                let height = self.number()?;
                self.close()?;
                Ok(Shape::Rect {
                    origin,
                    width,
                    height,
                    highlight: None,
                })
            }
            Opening::Word(b"oval") => {
                self.proper_whitespace()?;
                let origin = self.point()?;
                self.proper_whitespace()?;
                let width = self.number()?;
                self.proper_whitespace()?;
                let height = self.number()?;
                self.close()?;
                Ok(Shape::Oval {
                    origin,
                    width,
                    height,
                })
            }
            Opening::Word(b"text") => {
                self.proper_whitespace()?;
                let origin = self.point()?;
                self.proper_whitespace()?;
                let width = self.number()?;
                self.proper_whitespace()?;
                let height = self.number()?;
                self.close()?;
                Ok(Shape::Text {
                    origin,
                    width,
                    height,
                    background_color: None,
                    collapsible: false,
                    text_color: BLACK,
                })
            }
            Opening::Word(b"poly") => {
                let mut vertices = Vec::new();
                while self.whitespace().is_some() {
                    vertices.push(self.point()?);
                }
                self.close()?;
                Ok(Shape::Poly { vertices })
            }
            Opening::Word(b"line") => {
                self.proper_whitespace()?;
                let p_0 = self.point()?;
                self.proper_whitespace()?;
                let p_1 = self.point()?;
                self.close()?;
                Ok(Shape::Line {
                    endpoints: [p_0, p_1],
                    arrow: false,
                    color: BLACK,
                    width: 1,
                })
            }
            Opening::Word(other) => Err(AnnotationError::bad_word(other)),
        }
    }

    fn effect(&mut self, target: &mut MapArea<'_>) -> Result<Option<()>, AnnotationError> {
        match self.open()? {
            Opening::EndOfInput => Err(AnnotationError::EndOfInput),
            Opening::CloseParen => Ok(None),

            Opening::Word(b"none") => {
                target.border.kind = BorderKind::None;
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"xor") => {
                target.border.kind = BorderKind::Xor;
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"border") => {
                self.proper_whitespace()?;
                let color = self.color()?;
                target.border.kind = BorderKind::Color(color);
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"shadow_in") => {
                self.proper_whitespace()?;
                let thickness = self.number()?;
                target.border.kind = BorderKind::ShadowIn(thickness);
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"shadow_out") => {
                self.proper_whitespace()?;
                let thickness = self.number()?;
                target.border.kind = BorderKind::ShadowOut(thickness);
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"border_avis") => {
                target.border.always_visible = true;
                self.close()?;
                Ok(Some(()))
            }

            Opening::Word(b"hilite") => {
                self.proper_whitespace()?;
                let color = self.color()?;
                match &mut target.shape {
                    Shape::Rect {
                        highlight: highlight @ None,
                        ..
                    } => *highlight = Some(Highlight { color, opacity: 50 }),
                    _ => return Err(AnnotationError::InappropriateEffect("hilite")),
                }
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"opacity") => {
                self.proper_whitespace()?;
                let op = self.number()?;
                match &mut target.shape {
                    Shape::Rect {
                        highlight: Some(Highlight { opacity, .. }),
                        ..
                    } => *opacity = op,
                    _ => return Err(AnnotationError::InappropriateEffect("opacity")),
                }
                self.close()?;
                Ok(Some(()))
            }

            Opening::Word(b"arrow") => {
                match &mut target.shape {
                    Shape::Line { arrow, .. } => *arrow = true,
                    _ => return Err(AnnotationError::InappropriateEffect("arrow")),
                }
                self.close()?;
                Ok(Some(()))
            }

            Opening::Word(b"width") => {
                self.proper_whitespace()?;
                let n = self.number()?;
                match &mut target.shape {
                    Shape::Line { width, .. } => *width = n,
                    _ => return Err(AnnotationError::InappropriateEffect("width")),
                }
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"lineclr") => {
                self.proper_whitespace()?;
                let col = self.color()?;
                match &mut target.shape {
                    Shape::Line { color, .. } => *color = col,
                    _ => return Err(AnnotationError::InappropriateEffect("lineclr")),
                }
                self.close()?;
                Ok(Some(()))
            }

            Opening::Word(b"backclr") => {
                self.proper_whitespace()?;
                let color = self.color()?;
                match &mut target.shape {
                    Shape::Text {
                        background_color, ..
                    } => *background_color = Some(color),
                    _ => return Err(AnnotationError::InappropriateEffect("backclr")),
                }
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"textclr") => {
                self.proper_whitespace()?;
                let color = self.color()?;
                match &mut target.shape {
                    Shape::Text { text_color, .. } => *text_color = color,
                    _ => return Err(AnnotationError::InappropriateEffect("textclr")),
                }
                self.close()?;
                Ok(Some(()))
            }
            Opening::Word(b"pushpin") => {
                match &mut target.shape {
                    Shape::Text { collapsible, .. } => *collapsible = true,
                    _ => return Err(AnnotationError::InappropriateEffect("textclr")),
                }
                self.close()?;
                Ok(Some(()))
            }

            Opening::Word(other) => Err(AnnotationError::bad_word(other)),
        }
    }

    fn annotation(&mut self) -> Result<Option<Annotation<'a>>, AnnotationError> {
        match self.open()? {
            Opening::EndOfInput => Ok(None),
            Opening::CloseParen => Err(AnnotationError::UnmatchedParen),
            Opening::Word(b"background") => {
                self.proper_whitespace()?;
                let color = self.color()?;
                self.close()?;
                Ok(Some(Annotation::Background(color)))
            }
            Opening::Word(b"zoom") => {
                self.proper_whitespace()?;
                let val = self.zoom()?;
                self.close()?;
                Ok(Some(Annotation::Zoom(val)))
            }
            Opening::Word(b"mode") => {
                self.proper_whitespace()?;
                let val = self.mode()?;
                self.close()?;
                Ok(Some(Annotation::Mode(val)))
            }
            Opening::Word(b"align") => {
                self.proper_whitespace()?;
                let horiz = self.horiz_align()?;
                self.proper_whitespace()?;
                let vert = self.vert_align()?;
                self.close()?;
                Ok(Some(Annotation::Align { horiz, vert }))
            }
            Opening::Word(b"maparea") => {
                self.proper_whitespace()?;
                let link = self.link()?;
                self.proper_whitespace()?;
                let comment = self.escaped_str()?;
                self.proper_whitespace()?;
                let shape = self.shape()?;
                let mut area = MapArea {
                    link,
                    comment,
                    shape,
                    border: Border::default(),
                };
                // we don't require whitespace between effects since they're parenthesized
                while {
                    self.whitespace();
                    self.effect(&mut area)?.is_some()
                } {}
                self.close()?;
                Ok(Some(Annotation::MapArea(area)))
            }
            Opening::Word(b"phead") => {
                let mut strings = MarginStrings::default();
                while self.whitespace().is_some() && self.margin_string(&mut strings)?.is_some() {}
                self.close()?;
                Ok(Some(Annotation::Header(strings)))
            }
            Opening::Word(b"pfoot") => {
                let mut strings = MarginStrings::default();
                while self.whitespace().is_some() && self.margin_string(&mut strings)?.is_some() {}
                self.close()?;
                Ok(Some(Annotation::Footer(strings)))
            }
            Opening::Word(other) => Err(AnnotationError::bad_word(other)),
        }
    }
}
