use crate::annot::*;
use super::Error;

pub struct ParsingAnnots<'a> {
    full: &'a str,
    pos: usize,
}

impl<'a> ParsingAnnots<'a> {
    pub fn new(full: &'a str) -> Self {
        Self { full, pos: 0 }
    }

    fn remaining(&self) -> &'a str {
        &self.full[self.pos..]
    }

    fn token(&mut self) -> Result<Option<Token<'a>>, Error> {
        let (off, c, rest) = split_for_token(self.remaining());
        let (tok, i) = match c {
            None => return Ok(None),
            Some('(') => (Token::Open, 1),
            Some(')') => (Token::Close, 1),
            Some('#') => todo!(), // color
            Some('"') => todo!(), // quoted
            Some(a) if a.is_ascii_alphabetic() => todo!(), // word
            Some(d) if d.is_ascii_digit() => todo!(), // number
            _ => return Err(Error::placeholder()),
        };
        self.pos += off + i;
        Ok(Some(tok))
    }

    fn peek_token(&self) -> Result<Option<Token<'a>>, Error> {
        self.clone().token()
    }

    fn opening(&mut self) -> Result<Option<&'a str>, Error> {
        todo!()
    }

    pub fn parse_next(&mut self) -> Result<Option<Annot>, Error> {
        let kw = match self.opening()? {
            None => return Ok(None),
            Some(word) => word,
        };

        let annot = match kw {
            "background" => {
                let color = required(self.color())?;
                Annot::Background(color)
            }
            "zoom" => {
                let val = match required(self.word())? {
                    "stretch" => Zoom::Stretch,
                    "one2one" => Zoom::One2One,
                    "width" => Zoom::Width,
                    "page" => Zoom::Page,
                    _ => todo!(), // d123
                };
                Annot::Zoom(val)
            }
            "mode" => {
                let val = match required(self.word())? {
                    "color" => Mode::Color,
                    "bw" => Mode::Bw,
                    "fore" => Mode::Fore,
                    "black" => Mode::Black,
                    _ => return Err(Error::placeholder()),
                };
                Annot::Mode(val)
            }
            "align" => {
                let horiz = match required(self.word())? {
                    "left" => HorizAlign::Left,
                    "center" => HorizAlign::Center,
                    "right" => HorizAlign::Right,
                    // TODO default
                    _ => return Err(Error::placeholder()),
                };
                let vert = match required(self.word())? {
                    "top" => VertAlign::Top,
                    "center" => VertAlign::Center,
                    "bottom" => VertAlign::Bottom,
                    _ => return Err(Error::placeholder()),
                };
                Annot::Align { horiz, vert }
            }
            "maparea" => todo!(),
            "phead" => {
                let strings = self.margin_strings()?;
                Annot::Phead(strings)
            }
            "pfoot" => {
                let strings = self.margin_strings()?;
                Annot::Pfoot(strings)
            }
            "metadata" => {
                let mut pairs = Vec::new(),
                while let Some(k) = self.opening()? {
                    let v = required(self.quoted())?;
                    self.close()?;
                    pairs.push((Key::from_str(k), Quoted::from_raw(v)));
                }
                Annot::Metadata(pairs)
            }
            "xmp" => {
                let xmp = required(self.quoted())?;
                Annot::Xmp(xml)
            }
        };

        Ok(Some(annot))
    }
}

fn split_for_token(raw: &str) -> (usize, Option<char>, &str) {
    todo!()
}

enum Token<'a> {
    Open,
    Close,
    Color(Color),
    Quoted(&'a str),
    Word(&'a str),
    Number(u32),
}
