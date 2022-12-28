//! Representation of document annotations from the `ANTa` and `ANTz` chunks.

use core::fmt::{Display, Formatter};
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

struct Di<T>(T);

/// The error when attempting to construct a [`Key`] that contains invalid characters.
///
/// Implements [`std::error::Error`] if the `std` crate feature is enabled.
#[derive(Debug)]
pub struct KeyError;

impl Display for KeyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "invalid key")
    }
}

#[cfg(feature = "std")]
impl std::error::Error for KeyError {}

/// A string key occurring in the `metadata` annotation.
#[derive(Clone, Debug)]
pub struct Key(Arc<str>);

impl Key {
    /// Construct a new key.
    ///
    /// The string is restricted to ASCII alphanumeric characters, and must begin with an ASCII
    /// alphabetic character.
    pub fn new(s: &str) -> Result<Self, KeyError> {
        if s.chars().nth(0).map_or(false, |c| c.is_ascii_alphabetic()) &&
            s.chars().all(|c| c.is_ascii_alphanumeric()) {
            Ok(Self(s.into()))
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
    data: Arc<str>,
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
                    let _ = write!(&mut data, "\\{:03o}", np as u32);
                }
                other => data.push(other),
            }
        }
        Self { data: data.into() }
    }
}

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
#[derive(Clone, Debug, Default)]
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
#[derive(Clone, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Copy, Debug)]
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
#[derive(Clone, Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
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
