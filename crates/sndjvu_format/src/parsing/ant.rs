use super::{Error, error_placeholder};
use crate::annot::*;
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub struct Annotations<'a> {
    full: &'a [u8],
    pos: usize,
}

enum Token<'a> {
    Open,
    Close,
    Color(Color),
    Quoted(Quoted),
    Word(&'a [u8]),
    Number(u32),
}

fn parse_quoted(raw: &[u8]) -> Quoted {
    todo!()
}

fn parse_link_dest(raw: Quoted) -> LinkDest {
    todo!()
}

impl<'a> Annotations<'a> {
    pub(super) fn new(full: &'a [u8]) -> Self {
        Self { full, pos: 0 }
    }

    fn maybe_token(&mut self) -> Result<Option<Token<'a>>, Error> {
        let (i, c, rest) = match self.full[self.pos..].iter().position(|&c| c != b' ' && c != b'\t' && c != b'\r' && c != b'\n') {
            None => {
                self.pos = self.full.len();
                return Ok(None);
            }
            Some(x) => {
                let (&c, rest) = self.full[self.pos + x..].split_first().unwrap();
                (x, c, rest)
            }
        };
        let (tok, len) = match c {
            b'(' => (Token::Open, 1),
            b')' => (Token::Close, 1),
            b'#' => {
                let mut xs = rest.iter().copied();
                let mut channels = [0; 3];
                for chan in &mut channels {
                    for _ in 0..2 {
                        *chan = (*chan << 4) + match xs.next() {
                            Some(x @ b'0'..=b'9') => x - b'0',
                            Some(x @ b'a'..=b'f') => x - b'a' + 16,
                            Some(x @ b'A'..=b'F') => x - b'A' + 16,
                            _ => return Err(error_placeholder()),
                        };
                    }
                }
                let [r, g, b] = channels;
                (Token::Color(Color { r, g, b }), 7)
            }
            b'"' => {
                // note: only support the DjVuLibre quoting convention to start
                todo!()
            }
            a if a.is_ascii_alphabetic() => todo!(), // word/key
            d if d.is_ascii_digit() => todo!(), // number
            _ => return Err(error_placeholder()),
        };
        self.pos += i + len;
        Ok(Some(tok))
    }

    fn token(&mut self) -> Result<Token<'a>, Error> {
        match self.maybe_token()? {
            None => Err(error_placeholder()),
            Some(token) => Ok(token),
        }
    }

    fn maybe_open(&mut self) -> Result<Option<&'a [u8]>, Error> {
        let tok = match self.maybe_token()? {
            None => return Ok(None),
            Some(k) => k,
        };
        match tok {
            Token::Open => {},
            _ => return Err(error_placeholder()),
        }
        match self.maybe_token()? {
            Some(Token::Word(bare)) => Ok(Some(bare)),
            _ => return Err(error_placeholder()),
        }
    }

    fn open(&mut self) -> Result<&'a [u8], Error> {
        self.maybe_open()?.ok_or_else(error_placeholder)
    }

    fn color(&mut self) -> Result<Color, Error> {
        match self.token()? {
            Token::Color(color) => Ok(color),
            _ => Err(error_placeholder()),
        }
    }

    fn word(&mut self) -> Result<&'a [u8], Error> {
        match self.token()? {
            Token::Word(word) => Ok(word),
            _ => Err(error_placeholder()),
        }
    }

    fn close(&mut self) -> Result<(), Error> {
        match self.token()? {
            Token::Close => Ok(()),
            _ => Err(error_placeholder()),
        }
    }

    fn quoted(&mut self) -> Result<Quoted, Error> {
        self.maybe_quoted()?.ok_or_else(error_placeholder)
    }

    fn maybe_quoted(&mut self) -> Result<Option<Quoted>, Error> {
        match self.maybe_token()? {
            None => Ok(None),
            Some(Token::Quoted(quoted)) => Ok(Some(quoted)),
            _ => Err(error_placeholder()),
        }
    }

    fn margin_strings(&mut self) -> Result<MarginStrings, Error> {
        todo!()
    }

    fn number(&mut self) -> Result<u32, Error> {
        match self.token()? {
            Token::Number(number) => Ok(number),
            _ => Err(error_placeholder()),
        }
    }

    fn point(&mut self) -> Result<Point, Error> {
        self.maybe_point()?.ok_or_else(error_placeholder)
    }

    fn maybe_point(&mut self) -> Result<Option<Point>, Error> {
        let mut dup = self.clone();
        match dup.maybe_token()? {
            None => Ok(None),
            Some(Token::Number(x)) => match dup.maybe_token()? {
                None => Ok(None),
                Some(Token::Number(y)) => {
                    *self = dup;
                    Ok(Some(Point { x, y }))
                }
                _ => Err(error_placeholder()),
            }
            _ => Err(error_placeholder()),
        }
    }

    pub fn parse_next(&mut self) -> Result<Option<Annot>, Error> {
        let kw = match self.maybe_open()? {
            None => return Ok(None),
            Some(word) => word,
        };
        let annot = match kw {
            b"background" => {
                let color = self.color()?;
                Annot::Background(color)
            }
            b"zoom" => {
                let val = match self.word()? {
                    b"stretch" => Zoom::Stretch,
                    b"one2one" => Zoom::One2One,
                    b"width" => Zoom::Width,
                    b"page" => Zoom::Page,
                    &[b'd', ref rest @ ..] => todo!(),
                    _ => return Err(error_placeholder()),
                };
                Annot::Zoom(val)
            }
            b"mode" => {
                let val = match self.word()? {
                    b"color" => Mode::Color,
                    b"bw" => Mode::Bw,
                    b"fore" => Mode::Fore,
                    b"black" => Mode::Black,
                    _ => return Err(error_placeholder()),
                };
                Annot::Mode(val)
            }
            b"align" => {
                let horiz = match self.word()? {
                    b"left" => HorizAlign::Left,
                    b"center" | b"default" => HorizAlign::Center,
                    b"right" => HorizAlign::Right,
                    _ => return Err(error_placeholder()),
                };
                let vert = match self.word()? {
                    b"top" => VertAlign::Top,
                    b"center" | b"default" => VertAlign::Center,
                    b"bottom" => VertAlign::Bottom,
                    _ => return Err(error_placeholder()),
                };
                Annot::Align { horiz, vert }
            }

            b"maparea" => {
                let link = match self.token()? {
                    Token::Open => {
                        if self.word()? != b"url" {
                            return Err(error_placeholder());
                        }
                        let raw_dest = self.quoted()?;
                        let dest = parse_link_dest(raw_dest);
                        let target = self.quoted()?;
                        Link { dest, target: Some(target) }
                    }
                    Token::Quoted(raw_dest) => {
                        let dest = parse_link_dest(raw_dest);
                        Link { dest, target: None }
                    }
                    _ => return Err(error_placeholder()),
                };
                let comment = self.quoted()?;
                let shape = match self.open()? {
                    b"rect" => {
                        let origin = self.point()?;
                        let width = self.number()?;
                        let height = self.number()?;
                        Shape::Rect { origin, width, height, border_always_visible: false, highlight: None }
                    }
                    b"text" => {
                        let origin = self.point()?;
                        let width = self.number()?;
                        let height = self.number()?;
                        Shape::Text { origin, width, height, background_color: None, text_color: Color::BLACK, pushpin: false }
                    }
                    b"oval" => {
                        let origin = self.point()?;
                        let width = self.number()?;
                        let height = self.number()?;
                        Shape::Oval { origin, width, height, border_always_visible: false }
                    }
                    b"poly" => {
                        let mut vertices = Vec::new();
                        while let Some(pt) = self.maybe_point()? {
                            vertices.push(pt);
                        }
                        Shape::Poly { vertices, border_always_visible: false }
                    }
                    b"line" => {
                        let endpoints = [self.point()?, self.point()?];
                        Shape::Line { endpoints, arrow: false, width: 1, color: Color::BLACK }
                    }
                    _ => return Err(error_placeholder()),
                };
                self.close()?;
                let mut maparea = Maparea {
                    link,
                    comment,
                    shape,
                    border: Border::default(),
                };
                while let Some(word) = self.maybe_open()? {
                    // FIXME should we detect repeated/conflicting effects and error?
                    match word {
                        b"none" => maparea.border = Border::None,
                        b"xor" => maparea.border = Border::Xor,
                        b"border" => {
                            let color = self.color()?;
                            maparea.border = Border::Color(color);
                        }
                        b"shadow_in" => {
                            if !matches!(maparea.shape, Shape::Rect { .. }) {
                                return Err(error_placeholder());
                            }
                            let thickness = self.number()?;
                            maparea.border = Border::ShadowIn(thickness);
                        }
                        b"shadow_out" => {
                            if !matches!(maparea.shape, Shape::Rect { .. }) {
                                return Err(error_placeholder());
                            }
                            let thickness = self.number()?;
                            maparea.border = Border::ShadowOut(thickness);
                        }
                        b"shadow_ein" => {
                            if !matches!(maparea.shape, Shape::Rect { .. }) {
                                return Err(error_placeholder());
                            }
                            let thickness = self.number()?;
                            maparea.border = Border::ShadowEin(thickness);
                        }
                        b"shadow_eout" => {
                            if !matches!(maparea.shape, Shape::Rect { .. }) {
                                return Err(error_placeholder());
                            }
                            let thickness = self.number()?;
                            maparea.border = Border::ShadowEout(thickness);
                        }
                        b"border_avis" => {
                            match maparea.shape {
                                Shape::Rect { ref mut border_always_visible, .. }
                                | Shape::Oval { ref mut border_always_visible, .. }
                                | Shape::Poly { ref mut border_always_visible, ..} => *border_always_visible = true,
                                _ => return Err(error_placeholder()),
                            }
                        }

                        b"hilite" => {
                            let color = self.color()?;
                            match maparea.shape {
                                Shape::Rect { ref mut highlight, .. } => *highlight = Some(Highlight { color, opacity: 50 }),
                                _ => return Err(error_placeholder()),
                            }
                        }
                        b"opacity" => {
                            let n = self.number()?;
                            match maparea.shape {
                                Shape::Rect { highlight: Some(Highlight { ref mut opacity, .. }), .. } => *opacity = n,
                                _ => return Err(error_placeholder()),
                            }
                        }

                        b"arrow" => {
                            match maparea.shape {
                                Shape::Line { ref mut arrow, .. } => *arrow = true,
                                _ => return Err(error_placeholder()),
                            }
                        }
                        b"width" => {
                            let n = self.number()?;
                            match maparea.shape {
                                Shape::Line { ref mut width, .. } => *width = n,
                                _ => return Err(error_placeholder()),
                            }
                        }
                        b"lineclr" => {
                            let col = self.color()?;
                            match maparea.shape {
                                Shape::Line { ref mut color, .. } => *color = col,
                                _ => return Err(error_placeholder()),
                            }
                        }

                        b"backclr" => {
                            let color = self.color()?;
                            match maparea.shape {
                                Shape::Text { ref mut background_color, .. } => *background_color = Some(color),
                                _ => return Err(error_placeholder()),
                            }
                        }
                        b"textclr" => {
                            let color = self.color()?;
                            match maparea.shape {
                                Shape::Text { ref mut text_color, .. } => *text_color = color,
                                _ => return Err(error_placeholder()),
                            }
                        }
                        b"pushpin" => {
                            match maparea.shape {
                                Shape::Text { ref mut pushpin, .. } => *pushpin = true,
                                _ => return Err(error_placeholder()),
                            }
                        }
                        _ => return Err(error_placeholder()),
                    }
                    self.close()?;
                }
                Annot::Maparea(maparea)
            }

            b"phead" => {
                let strings = self.margin_strings()?;
                Annot::Phead(strings)
            }
            b"pfoot" => {
                let strings = self.margin_strings()?;
                Annot::Pfoot(strings)
            }

            b"metadata" => {
                let mut pairs = Vec::new();
                while let Some(key) = self.maybe_open()? {
                    let val = self.quoted()?;
                    self.close()?;
                    todo!() // push the pair
                }
                Annot::Metadata(pairs)
            }
            b"xmp" => {
                let xml = self.quoted()?;
                Annot::Xmp(xml)
            }
            _ => return Err(error_placeholder()),
        };

        self.close()?;
        Ok(Some(annot))
    }
}
