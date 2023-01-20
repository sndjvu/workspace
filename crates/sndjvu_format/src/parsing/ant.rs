use crate::annot::*;
use super::Error;
use alloc::vec::Vec;

/// Fallible iterator for parsing the annotations from a string.
#[derive(Clone, Debug)]
pub struct ParsingAnnots<'a> {
    full: &'a str,
    quoting: QuotingConvention,
    pos: usize,
}

#[derive(Clone)]
struct Rest<'a> {
    full: &'a str,
    quoting: QuotingConvention,
    pos: usize,
}

fn split_for_token(raw: &str) -> (usize, Option<char>, &str) {
    let (i, c) = raw.char_indices()
        .find(|&(_, x)| !" \t\r\n".contains(x))
        .map_or((raw.len(), None), |(i, c)| (i, Some(c)));
    (i, c, &raw[i..])
}

impl<'a> Rest<'a> {
    fn token(&mut self) -> Result<Option<Token<'a>>, Error> {
        let (off, Some(c), rest) = split_for_token(&self.full[self.pos..]) else {
            return Ok(None)
        };
        let (tok, i) = match c {
            '(' => (Token::Open, 1),
            ')' => (Token::Close, 1),
            '#' => {
                let mut channels = [0; 3];
                let mut chars = rest.chars().skip(1);
                for chan in &mut channels {
                    for _ in 0..2 {
                        let Some(c) = chars.next() else {
                            return Err(Error::placeholder())
                        };
                        let n = match c {
                            '0'..='9' => c as u8 - b'0',
                            'a'..='f' => c as u8 - b'a' + 10,
                            'A'..='F' => c as u8 - b'A' + 10,
                            _ => return Err(Error::placeholder()),
                        };
                        *chan = (*chan << 4) + n;
                    }
                }
                let [r, g, b] = channels;
                (Token::Color(Color { r, g, b }), 1 + 6)
            }
            '"' => {
                let mut count = 0;
                let mut end = None;
                for (i, c) in rest.char_indices() {
                    if i > 0 && c == '"' && count % 2 == 0 &&
                        (self.quoting == QuotingConvention::Djvulibre || count == 0)
                    {
                        end = Some(i);
                        break;
                    } else if c == '\\' {
                        count += 1;
                    } else {
                        count = 0;
                    }
                }
                let Some(i) = end else {
                    return Err(Error::placeholder())
                };
                (Token::Quoted(Quoted::new_raw(&rest[1..i], 0, self.quoting)), 1 + i + 1)
            }
            a if a.is_ascii_alphabetic() => {
                let i = rest.char_indices()
                    .find(|&(_, c)| !c.is_ascii_alphanumeric() && c != '_')
                    .map_or(rest.len(), |(i, _)| i);
                (Token::Word(&rest[..i]), i)
            }
            d if d.is_ascii_digit() => {
                let i = rest.char_indices()
                    .find(|&(_, c)| !c.is_ascii_digit())
                    .map_or(rest.len(), |(i, _)| i);
                let Ok(n) = u32::from_str_radix(&rest[..i], 10) else {
                    return Err(Error::placeholder())
                };
                (Token::Number(n), i)
            }
            _ => {
                return Err(Error::placeholder())
            }
        };
        self.pos += off + i;
        Ok(Some(tok))
    }

    fn peek_token(&self) -> Result<Option<Token<'a>>, Error> {
        self.clone().token()
    }

    fn opening(&mut self) -> Result<Option<&'a str>, Error> {
        match self.peek_token()? {
            None | Some(Token::Close) => return Ok(None),
            Some(Token::Open) => {
                let _ = self.token();
            }
            _ => return Err(Error::placeholder()),
        }
        let Some(Token::Word(s)) = self.token()? else {
            return Err(Error::placeholder())
        };
        Ok(Some(s))
    }

    fn margin_strings(&mut self) -> Result<MarginStrings, Error> {
        let mut strings = MarginStrings::default();
        while let Ok(Some(Token::Quoted(mut s))) = self.peek_token() {
            let _ = self.token();
            if s.data.starts_with("left::") {
                s.starts_at = 6;
                strings.left = Some(s);
            } else if s.data.starts_with("center::") {
                s.starts_at = 8;
                strings.center = Some(s);
            } else if s.data.starts_with("right::") {
                s.starts_at = 7;
                strings.right = Some(s);
            }
        }
        Ok(strings)
    }

    fn two_numbers(&mut self) -> Result<[u32; 2], Error> {
        let Some(Token::Number(n0)) = self.token()? else {
            return Err(Error::placeholder())
        };
        let Some(Token::Number(n1)) = self.token()? else {
            return Err(Error::placeholder())
        };
        Ok([n0, n1])
    }

    fn close(&mut self) -> Result<(), Error> {
        let Some(Token::Close) = self.token()? else {
            return Err(Error::placeholder())
        };
        Ok(())
    }

    fn annot(&mut self) -> Result<Option<Annot>, Error> {
        let kw = match self.opening()? {
            None => return Ok(None),
            Some(word) => word,
        };

        let annot = match kw {
            "background" => {
                let Some(Token::Color(color)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                Annot::Background(color)
            }
            "zoom" => {
                let Some(Token::Word(word)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                let val = match word {
                    "stretch" => Zoom::Stretch,
                    "one2one" => Zoom::One2One,
                    "width" => Zoom::Width,
                    "page" => Zoom::Page,
                    w if w.starts_with("d")
                        && w.len() > 1
                        && w[1..].chars().all(|c| c.is_ascii_digit()) => {
                        let n = &w[1..];
                        Zoom::D(u32::from_str_radix(n, 10).unwrap())
                    }
                    _ => return Err(Error::placeholder()),
                };
                Annot::Zoom(val)
            }
            "mode" => {
                let Some(Token::Word(word)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                let val = match word {
                    "color" => Mode::Color,
                    "bw" => Mode::Bw,
                    "fore" => Mode::Fore,
                    "black" => Mode::Black,
                    _ => return Err(Error::placeholder()),
                };
                Annot::Mode(val)
            }
            "align" => {
                let Some(Token::Word(word)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                let horiz = match word {
                    "left" => HorizAlign::Left,
                    "center" => HorizAlign::Center,
                    "right" => HorizAlign::Right,
                    // TODO default
                    _ => return Err(Error::placeholder()),
                };
                let Some(Token::Word(word)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                let vert = match word {
                    "top" => VertAlign::Top,
                    "center" => VertAlign::Center,
                    "bottom" => VertAlign::Bottom,
                    // TODO default
                    _ => return Err(Error::placeholder()),
                };
                Annot::Align { horiz, vert }
            }
            "maparea" => {
                let link = match self.token()? {
                    Some(Token::Open) => {
                        let Some(Token::Word("url")) = self.token()? else {
                            return Err(Error::placeholder())
                        };
                        let Some(Token::Quoted(dest)) = self.token()? else {
                            return Err(Error::placeholder())
                        };
                        let Some(Token::Quoted(target)) = self.token()? else {
                            return Err(Error::placeholder())
                        };
                        Link { dest, target: Some(target) }
                    }
                    Some(Token::Quoted(dest)) => Link { dest, target: None },
                    _ => return Err(Error::placeholder()),
                };

                let Some(Token::Quoted(comment)) = self.token()? else {
                    return Err(Error::placeholder())
                };

                let Some(kind) = self.opening()? else {
                    return Err(Error::placeholder())
                };
                let mut shape = match kind {
                    "rect" => {
                        let [x, y] = self.two_numbers()?;
                        let [width, height] = self.two_numbers()?;
                        Shape::Rect {
                            origin: Point { x, y },
                            width,
                            height,
                            border_always_visible: false,
                            highlight: None,
                        }
                    }
                    "oval" => {
                        let [x, y] = self.two_numbers()?;
                        let [width, height] = self.two_numbers()?;
                        Shape::Oval {
                            origin: Point { x, y },
                            width,
                            height,
                            border_always_visible: false,
                        }
                    }
                    "text" => {
                        let [x, y] = self.two_numbers()?;
                        let [width, height] = self.two_numbers()?;
                        Shape::Text {
                            origin: Point { x, y },
                            width,
                            height,
                            background_color: None,
                            text_color: Color::BLACK,
                            pushpin: false,
                        }
                    }
                    "poly" => {
                        let mut vertices = Vec::new();
                        while let Ok(Some(Token::Number(_))) = self.peek_token() {
                            let [x, y] = self.two_numbers()?;
                            vertices.push(Point { x, y });
                        }
                        Shape::Poly { vertices, border_always_visible: false }
                    }
                    "line" => {
                        let [x, y] = self.two_numbers()?;
                        let p0 = Point { x, y };
                        let [x, y] = self.two_numbers()?;
                        let p1 = Point { x, y };
                        Shape::Line {
                            endpoints: [p0, p1],
                            arrow: false,
                            width: 1,
                            color: Color::BLACK,
                        }
                    }
                    _ => return Err(Error::placeholder()),
                };
                self.close()?;
                let mut border = Border::None;

                while let Some(w) = self.opening()? {
                    match w {
                        "none" => {
                            border = Border::None;
                        }
                        "xor" => {
                            border = Border::Xor;
                        }
                        "border" => {
                            let Some(Token::Color(col)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            border = Border::Color(col);
                        }
                        "shadow_in" => {
                            let Some(Token::Number(thickness)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            border = Border::ShadowIn(thickness);
                        }
                        "shadow_out" => {
                            let Some(Token::Number(thickness)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            border = Border::ShadowOut(thickness);
                        }
                        "shadow_ein" => {
                            let Some(Token::Number(thickness)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            border = Border::ShadowEin(thickness);
                        }
                        "shadow_eout" => {
                            let Some(Token::Number(thickness)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            border = Border::ShadowEout(thickness);
                        }
                        "border_avis" => {
                            let (Shape::Rect { ref mut border_always_visible, .. } |
                                 Shape::Oval { ref mut border_always_visible, .. } |
                                 Shape::Poly { ref mut border_always_visible, .. }) = shape
                            else {
                                return Err(Error::placeholder())
                            };
                            *border_always_visible = true;
                        }
                        "hilite" => {
                            let Some(Token::Color(col)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Rect { ref mut highlight, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            highlight.get_or_insert_with(|| Highlight { color: Color::WHATEVER, opacity: 50 }).color = col;
                        }
                        "opacity" => {
                            let Some(Token::Number(n)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Rect { ref mut highlight, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            highlight.get_or_insert_with(|| Highlight { color: Color::WHATEVER, opacity: 50 }).opacity = n;
                        }
                        "arrow" => {
                            let Shape::Line { ref mut arrow, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *arrow = true;
                        }
                        "width" => {
                            let Some(Token::Number(n)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Line { ref mut width, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *width = n;
                        }
                        "lineclr" => {
                            let Some(Token::Color(col)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Line { ref mut color, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *color = col;
                        }
                        "backclr" => {
                            let Some(Token::Color(col)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Text { ref mut background_color, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *background_color = Some(col);
                        }
                        "textclr" => {
                            let Some(Token::Color(col)) = self.token()? else {
                                return Err(Error::placeholder())
                            };
                            let Shape::Text { ref mut text_color, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *text_color = col;
                        }
                        "pushpin" => {
                            let Shape::Text { ref mut pushpin, .. } = shape else {
                                return Err(Error::placeholder())
                            };
                            *pushpin = true;
                        }
                        _ => return Err(Error::placeholder()),
                    }

                    self.close()?;
                }

                Annot::Maparea(Maparea { link, comment, shape, border })
            }
            "phead" => {
                let strings = self.margin_strings()?;
                Annot::Phead(strings)
            }
            "pfoot" => {
                let strings = self.margin_strings()?;
                Annot::Pfoot(strings)
            }
            "metadata" => {
                let mut pairs = Vec::new();
                while let Some(k) = self.opening()? {
                    let Some(Token::Quoted(v)) = self.token()? else {
                        return Err(Error::placeholder())
                    };
                    self.close()?;
                    pairs.push((Key::new_unchecked(k), v));
                }
                Annot::Metadata(pairs)
            }
            "xmp" => {
                let Some(Token::Quoted(xml)) = self.token()? else {
                    return Err(Error::placeholder())
                };
                Annot::Xmp(xml)
            }
            _ => return Err(Error::placeholder()),
        };

        self.close()?;

        Ok(Some(annot))
    }
}

impl<'a> ParsingAnnots<'a> {
    pub(crate) fn new(full: &'a str, quoting: QuotingConvention) -> Self {
        Self { full, quoting, pos: 0 }
    }

    fn rest(&self) -> Rest<'a> {
        Rest { full: &self.full[self.pos..], quoting: self.quoting, pos: 0 }
    }

    pub fn parse_next(&mut self) -> Result<Option<Annot>, Error> {
        let mut rest = self.rest();
        let annot = rest.annot()?;
        self.pos += rest.pos;
        Ok(annot)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Token<'a> {
    Open,
    Close,
    Color(Color),
    Quoted(Quoted),
    Word(&'a str),
    Number(u32),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize() -> Result<(), Error> {
        let full = "(foo bar \"baz\\0\\f\" #ABC123 175)";
        let mut rest = Rest { full, pos: 0, quoting: QuotingConvention::Djvulibre };
        let mut tokens = alloc::vec![];
        while let Some(tok) = rest.token()? {
            tokens.push(tok);
        }
        assert_eq!(
            &tokens[..],
            &[
                Token::Open,
                Token::Word("foo"),
                Token::Word("bar"),
                Token::Quoted(Quoted::new("baz\0\u{c}")),
                Token::Color(Color { r: 0xab, g: 0xc1, b: 0x23 }),
                Token::Number(175),
                Token::Close,
            ],
        );
        Ok(())
    }

    #[test]
    fn parse() -> Result<(), Error> {
        let full = "(background #FFFFFF) (zoom page) (mode bw) (align center top) \
                    (maparea \"http://lizardtech.com/\" \"Here is a rectangular hyperlink\" \
                    (rect 543 2859 408 183) (xor))";
        let mut annots = alloc::vec![];
        let mut parsing = ParsingAnnots::new(full, QuotingConvention::Djvulibre);
        while let Some(annot) = parsing.parse_next()? {
            annots.push(annot);
        }
        assert_eq!(
            &annots[..],
            &[
                Annot::Background(Color { r: 0xff, g: 0xff, b: 0xff }),
                Zoom::Page.into(),
                Mode::Bw.into(),
                Annot::Align { horiz: HorizAlign::Center, vert: VertAlign::Top },
                Maparea {
                    link: Link { dest: Quoted::new("http://lizardtech.com/"), target: None },
                    comment: Quoted::new("Here is a rectangular hyperlink"),
                    shape: Shape::Rect {
                        origin: Point { x: 543, y: 2859 },
                        width: 408,
                        height: 183,
                        border_always_visible: false,
                        highlight: None,
                    },
                    border: Border::Xor,
                }.into(),
            ],
        );
        Ok(())
    }
}
