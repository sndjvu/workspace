//! Representation of document annotations from the `ANTa` and `ANTz` chunks.

use core::fmt::{Display, Formatter};
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

struct Di<T>(T);

/// The error when attempting to construct a [`Key`] that contains invalid characters.
///
/// Implements [`std::error::Error`] if the `std` crate feature is enabled.
#[derive(Clone, Debug)]
pub struct KeyError;

impl Display for KeyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "invalid key")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for KeyError {}

/// A string key occurring in the `metadata` annotation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Key(Arc<str>);

impl Key {
    pub(crate) fn new_unchecked(s: &str) -> Self {
        Self(s.into())
    }

    /// Construct a new key.
    ///
    /// The string is restricted to ASCII alphanumeric characters, and must begin with an ASCII
    /// alphabetic character.
    pub fn new(s: &str) -> Result<Self, KeyError> {
        if s.chars().next().map_or(false, |c| c.is_ascii_alphabetic()) &&
            s.chars().all(|c| c.is_ascii_alphanumeric()) {
            Ok(Self::new_unchecked(s))
        } else {
            Err(KeyError)
        }
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A quoted string occurring in an annotation.
#[derive(Clone, Debug)]
pub struct Quoted {
    // data is stored in *escaped* repr, without surrounding quotes
    pub(crate) data: Arc<str>,
    pub(crate) starts_at: usize,
}

impl Quoted {
    /// Construct a new quoted string.
    pub fn new(s: &str) -> Self {
        use core::fmt::Write;

        let mut data = String::new();
        for c in s.chars() {
            match c {
                '"' => data.push_str("\\\""),
                '\\' => data.push_str("\\\\"),
                np if np < ' ' || np == '\x7f' => {
                    let _ = write!(&mut data, "\\{:03o}", np as u8);
                }
                other => data.push(other),
            }
        }
        Self { data: data.into(), starts_at: 0 }
    }

    pub(crate) fn new_raw(s: &str, starts_at: usize) -> Self {
        Self { data: s.into(), starts_at }
    }

    pub fn scalars(&self) -> Scalars<'_> {
        Scalars::new(&self.data[self.starts_at..])
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scalar {
    Char(char),
    Byte(u8),
}

pub struct Scalars<'a> {
    s: &'a str,
    gadget: utf8::Gadget,
}

impl<'a> Scalars<'a> {
    fn new(s: &'a str) -> Self {
        Self { s, gadget: utf8::Gadget::new() }
    }

    /// Get one UTF-8 scalar or one escape sequence, without coalescing.
    fn next_raw(&mut self) -> Option<Scalar> {
        fn is_ascii_octdigit(x: u8) -> bool {
            (b'0'..b'8').contains(&x)
        }

        let (ix, sc) = match self.s.as_bytes() {
            // octal escapes
            &[b'\\', a, b, c, ..] if is_ascii_octdigit(a) && is_ascii_octdigit(b) && is_ascii_octdigit(c) => {
                (4, Some(Scalar::Byte(u8::from_str_radix(&self.s[1..4], 8).unwrap())))
            }
            &[b'\\', a, b, ..] if is_ascii_octdigit(a) && is_ascii_octdigit(b) => {
                (3, Some(Scalar::Byte(u8::from_str_radix(&self.s[1..3], 8).unwrap())))
            }
            &[b'\\', a, ..] if is_ascii_octdigit(a) => {
                (2, Some(Scalar::Byte(u8::from_str_radix(&self.s[1..2], 8).unwrap())))
            }
            // conventional escapes
            &[b'\\', b'a', ..] => {
                (2, Some(Scalar::Char('\u{7}')))
            }
            &[b'\\', b'b', ..] => {
                (2, Some(Scalar::Char('\u{8}')))
            }
            &[b'\\', b't', ..] => {
                (2, Some(Scalar::Char('\t')))
            }
            &[b'\\', b'n', ..] => {
                (2, Some(Scalar::Char('\n')))
            }
            &[b'\\', b'v', ..] => {
                (2, Some(Scalar::Char('\u{b}')))
            }
            &[b'\\', b'f', ..] => {
                (2, Some(Scalar::Char('\u{c}')))
            }
            &[b'\\', b'r', ..] => {
                (2, Some(Scalar::Char('\r')))
            }
            // non-escape
            _ => {
                let x = self.s.chars().next();
                (x.map_or(0, char::len_utf8), x.map(Scalar::Char))
            }
        };
        self.s = &self.s[ix..];
        sc
    }

    fn is_done(&self) -> bool {
        self.s.is_empty() && self.gadget.is_done()
    }
}

impl<'a> Iterator for Scalars<'a> {
    type Item = Scalar;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(byte) = self.gadget.get() {
                return Some(Scalar::Byte(byte));
            }
            match self.next_raw()? {
                Scalar::Byte(b) => {
                    let Some(sc) = self.gadget.put(b) else {
                        continue
                    };
                    return Some(sc);
                }
                Scalar::Char(c) => {
                    self.gadget.fence();
                    return Some(Scalar::Char(c));
                }
            }
        }
    }
}

impl PartialEq<Quoted> for Quoted {
    fn eq(&self, other: &Quoted) -> bool {
        let mut left = self.scalars();
        let mut right = other.scalars();
        let b = left.by_ref().zip(right.by_ref()).all(|(l, r)| l == r);
        left.is_done() && right.is_done() && b
    }
}

impl PartialEq<str> for Quoted {
    fn eq(&self, other: &str) -> bool {
        let mut left = self.scalars();
        let mut right = other.chars().map(Scalar::Char);
        let b = left.by_ref().zip(right.by_ref()).all(|(l, r)| l == r);
        left.is_done() && right.next() == None && b
    }
}

impl Eq for Quoted {}

impl Display for Quoted {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let s = &*self.data;
        if f.alternate() {
            write!(f, "{s}")
        } else {
            write!(f, "\"{s}\"")
        }
    }
}

/// Arguments to the (deprecated) `phead` and `pfoot` annotations.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MarginStrings {
    pub left: Option<Quoted>,
    pub center: Option<Quoted>,
    pub right: Option<Quoted>,
}

impl<'a> Display for Di<&'a MarginStrings> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut pad = "";
        if let Some(ref left) = self.0.left {
            write!(f, "\"left::{left:#}\"")?;
            pad = " ";
        }
        if let Some(ref center) = self.0.center {
            write!(f, "{pad}\"center::{center:#}\"")?;
            pad = " ";
        }
        if let Some(ref right) = self.0.right {
            write!(f, "{pad}\"right::{right:#}\"")?;
        }
        Ok(())
    }
}

/// A color from the RGB8 color space.
///
/// The `Display` implementation uses the notation that appears in DjVu annotations, e.g.
/// `#0BFC36`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Color {
    pub const BLACK: Self = Self { r: 0, g: 0, b: 0 };
    pub(crate) const WHATEVER: Self = Self { r: 0x18, g: 0x8c, b: 0x1d };
}

impl Display for Color {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let Self { r, g, b } = *self;
        write!(f, "#{r:02X}{g:02X}{b:02X}")
    }
}

/// The argument to the [`zoom`](Annot::Zoom) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Zoom {
    Stretch,
    One2One,
    Width,
    Page,
    D(u32),
}

impl Display for Zoom {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Stretch => write!(f, "stretch"),
            Self::One2One => write!(f, "one2one"),
            Self::Width => write!(f, "width"),
            Self::Page => write!(f, "page"),
            Self::D(x) => write!(f, "d{x}"),
        }
    }
}

/// The argument to the [`mode`](Annot::Mode) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Color,
    Bw,
    Fore,
    Black,
}

impl Display for Mode {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Color => write!(f, "color"),
            Self::Bw => write!(f, "bw"),
            Self::Fore => write!(f, "fore"),
            Self::Black => write!(f, "black"),
        }
    }
}

/// The first argument to the [`align`](Annot::Align) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HorizAlign {
    Left,
    Center,
    Right,
}

impl Display for HorizAlign {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Left => write!(f, "left"),
            Self::Center => write!(f, "center"),
            Self::Right => write!(f, "right"),
        }
    }
}

/// The second argument to the [`align`](Annot::Align) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VertAlign {
    Top,
    Center,
    Bottom,
}

impl Display for VertAlign {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Top => write!(f, "top"),
            Self::Center => write!(f, "center"),
            Self::Bottom => write!(f, "bottom"),
        }
    }
}

/// The first argument to the [`maparea`](Maparea) annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Link {
    pub dest: Quoted,
    pub target: Option<Quoted>,
}

impl Display for Link {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let dest = &self.dest;
        if let Some(ref target) = self.target {
            write!(f, "(url {dest} {target})")
        } else {
            write!(f, "{dest}")
        }
    }
}

/// A cartesian coordinate pair, as used in the [`maparea`](Maparea) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Point {
    pub x: u32,
    pub y: u32,
}

impl Display for Di<Point> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let Self(Point { x, y }) = *self;
        write!(f, "{x} {y}")
    }
}

/// Highlighting of a `rect` in the [`maparea`](Maparea) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Highlight {
    pub color: Color,
    pub opacity: u32,
}

impl Display for Di<Highlight> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let Self(Highlight { color, opacity }) = *self;
        write!(f, "(hilite {color}) (opacity {opacity})")
    }
}

/// Border format of a [`maparea`](Maparea) annotation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Border {
    None,
    Xor,
    Color(Color),
    ShadowIn(u32),
    ShadowOut(u32),
    ShadowEin(u32),
    ShadowEout(u32),
}

impl Default for Border {
    fn default() -> Self {
        Self::None
    }
}

impl From<Color> for Border {
    fn from(x: Color) -> Self {
        Self::Color(x)
    }
}

impl Display for Border {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Border::None => write!(f, "(none)"),
            Border::Xor => write!(f, "(xor)"),
            Border::Color(color) => write!(f, "(border {color})"),
            Border::ShadowIn(ref x) => write!(f, "(shadow_in {x})"),
            Border::ShadowOut(ref x) => write!(f, "(shadow_out {x})"),
            Border::ShadowEin(ref x) => write!(f, "(shadow_ein {x})"),
            Border::ShadowEout(ref x) => write!(f, "(shadow_eout {x})"),
        }
    }
}

/// Shape and "effect" data associated with a [`maparea`](Maparea) annotation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Shape {
    Rect {
        origin: Point,
        width: u32,
        height: u32,
        border_always_visible: bool,
        highlight: Option<Highlight>,
    },
    Oval {
        origin: Point,
        width: u32,
        height: u32,
        border_always_visible: bool,
    },
    Text {
        origin: Point,
        width: u32,
        height: u32,
        background_color: Option<Color>,
        text_color: Color,
        pushpin: bool,
    },
    Poly {
        vertices: Vec<Point>,
        border_always_visible: bool,
    },
    Line {
        endpoints: [Point; 2],
        arrow: bool,
        width: u32,
        color: Color,
    },
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Rect { origin, width, height, border_always_visible, highlight } => {
                write!(f, "(rect {} {width} {height})", Di(origin))?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
                if let Some(Highlight { color, opacity }) = highlight {
                    write!(f, " (hilite {color}) (opacity {opacity})")?;
                }
            }
            Self::Oval { origin, width, height, border_always_visible } => {
                write!(f, "(oval {} {width} {height})", Di(origin))?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
            }
            Self::Text { origin, width, height, background_color, text_color, pushpin } => {
                write!(f, "(text {} {width} {height})", Di(origin))?;
                if let Some(color) = background_color {
                    write!(f, " (backclr {color})")?;
                }
                write!(f, " (textclr {text_color})")?;
                if pushpin {
                    write!(f, " (pushpin)")?;
                }
            }
            Self::Poly { ref vertices, border_always_visible } => {
                write!(f, "(poly")?;
                for &point in vertices {
                    write!(f, " {}", Di(point))?;
                }
                write!(f, ")")?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
            }
            Self::Line { endpoints: [start, end], arrow, width, color } => {
                write!(f, "(line {} {})", Di(start), Di(end))?;
                if arrow {
                    write!(f, " (arrow)")?;
                }
                write!(f, " (width {width})")?;
                write!(f, " (lineclr {color})")?;
            }
        }
        Ok(())
    }
}

/// A `maparea` annotation.
///
/// ```
/// # use sndjvu_format::annot::*;
/// let dest = Quoted::new("http://www.lizardtech.com/");
/// let comment = Quoted::new("Here is a rectangular hyperlink");
/// let shape = Shape::Rect {
///     origin: Point { x: 543, y: 2859 },
///     width: 408,
///     height: 183,
///     border_always_visible: false,
///     highlight: None,
/// };
/// let annot: Annot = Maparea {
///     link: Link { dest, target: None },
///     comment,
///     shape,
///     border: Border::Xor,
/// }.into();
/// assert_eq!(
///     annot.to_string(),
///     "(maparea \
///         \"http://www.lizardtech.com/\" \
///         \"Here is a rectangular hyperlink\" \
///         (rect 543 2859 408 183) (xor))",
/// );
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Maparea {
    pub link: Link,
    pub comment: Quoted,
    pub shape: Shape,
    pub border: Border,
}

/// A single annotation.
///
/// ```
/// # use sndjvu_format::annot::*;
/// let annots = [
///     Annot::Background(Color { r: 0xff, g: 0xff, b: 0xff }),
///     Zoom::Page.into(),
///     Mode::Bw.into(),
///     Annot::Align { horiz: HorizAlign::Center, vert: VertAlign::Top },
/// ];
/// let s = annots.iter().map(Annot::to_string).collect::<Vec<_>>().join(" ");
/// assert_eq!(
///     s,
///     "(background #FFFFFF) (zoom page) (mode bw) (align center top)",
/// );
/// ```
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Annot {
    Background(Color),
    Zoom(Zoom),
    Mode(Mode),
    Align { horiz: HorizAlign, vert: VertAlign },
    Maparea(Maparea),
    /// The `phead` annotation should not be used in new DjVu documents.
    Phead(MarginStrings),
    /// The `pfoot` annotation should not be used in new DjVu documents.
    Pfoot(MarginStrings),
    Metadata(Vec<(Key, Quoted)>),
    Xmp(Quoted),
}

impl From<Zoom> for Annot {
    fn from(x: Zoom) -> Self {
        Self::Zoom(x)
    }
}

impl From<Mode> for Annot {
    fn from(x: Mode) -> Self {
        Self::Mode(x)
    }
}

impl From<Maparea> for Annot {
    fn from(x: Maparea) -> Self {
        Self::Maparea(x)
    }
}

impl Display for Annot {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match *self {
            Self::Background(color) => write!(f, "(background {color})"),
            Self::Zoom(zoom) => write!(f, "(zoom {zoom})"),
            Self::Mode(mode) => write!(f, "(mode {mode})"),
            Self::Align { horiz, vert } => write!(f, "(align {horiz} {vert})"),
            Self::Maparea(Maparea { ref link, ref comment, ref shape, border }) => {
                write!(f, "(maparea {link} {comment} {shape} {border})")
            }
            Self::Phead(ref strings) => write!(f, "(phead {})", Di(strings)),
            Self::Pfoot(ref strings) => write!(f, "(pfoot {})", Di(strings)),
            Self::Metadata(ref pairs) => {
                write!(f, "(metadata")?;
                for &(ref key, ref val) in pairs {
                    write!(f, " ({key} {val})")?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Self::Xmp(ref xml) => write!(f, "(xmp {xml})"),
        }
    }
}

mod utf8 {
    use core::mem::replace;
    use super::Scalar;

    // The FSM is copied from Andrew Gallant's bstr library, with thanks.

    type State = usize;

    const ACCEPT: State = 12;
    const REJECT: State = 0;
    const MUNCHING: State = usize::MAX;

    const CLASSES: [u8; 256] = [
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
       7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
       8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
      10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,
    ];

    const STATES_FORWARD: &'static [u8] = &[
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      12, 0, 24, 36, 60, 96, 84, 0, 0, 0, 48, 72,
      0, 12, 0, 0, 0, 0, 0, 12, 0, 12, 0, 0,
      0, 24, 0, 0, 0, 0, 0, 24, 0, 24, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 24, 0, 0, 0, 0,
      0, 24, 0, 0, 0, 0, 0, 0, 0, 24, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 36, 0, 36, 0, 0,
      0, 36, 0, 0, 0, 0, 0, 36, 0, 36, 0, 0,
      0, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];

    fn decode_step(state: &mut State, cp: &mut u32, b: u8) {
        let class = CLASSES[b as usize];
        if *state == ACCEPT {
            *cp = (0xFF >> class) & (b as u32);
        } else {
            *cp = (b as u32 & 0b111111) | (*cp << 6);
        }
        *state = STATES_FORWARD[*state + class as usize] as usize;
    }

    pub struct Gadget {
        bytes: [u8; 4],
        count: usize,
        state: State,
        cp: u32,
    }

    impl Gadget {
        pub fn new() -> Self {
            Self {
                bytes: [0xff; 4],
                count: 0,
                state: ACCEPT,
                cp: 0,
            }
        }

        pub fn put(&mut self, b: u8) -> Option<Scalar> {
            self.bytes[self.count] = b;
            self.count += 1;
            decode_step(&mut self.state, &mut self.cp, b);
            match self.state {
                ACCEPT => {
                    let c = char::from_u32(self.cp).unwrap();
                    *self = Self::new();
                    Some(Scalar::Char(c))
                }
                REJECT => {
                    self.count -= 1;
                    self.cp = 0;
                    let x = replace(&mut self.bytes[0], 0xff);
                    self.bytes.rotate_left(1);
                    self.fence();
                    Some(Scalar::Byte(x))
                }
                _ => None,
            }
        }

        pub fn get(&mut self) -> Option<u8> {
            (self.state == MUNCHING).then(|| {
                self.count -= 1;
                let x = replace(&mut self.bytes[0], 0xff);
                self.bytes.rotate_left(1);
                self.fence();
                x
            })
        }

        pub fn fence(&mut self) {
            self.state = if self.count > 0 { MUNCHING } else { ACCEPT };
        }

        pub fn is_done(&self) -> bool {
            self.count == 0
        }
    }
}
