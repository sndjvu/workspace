use core::fmt::{Display, Formatter};
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Clone, Debug)]
pub struct Key(String);

impl Display for Key {
    fn fmt(&self, _f: &mut Formatter<'_>) -> core::fmt::Result {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct Quoted {
    _data: Vec<u8>,
}

impl Display for Quoted {
    fn fmt(&self, _f: &mut Formatter<'_>) -> core::fmt::Result {
        todo!()
    }
}

/// The arguments to a [`phead`](Annot::Phead) or [`pfoot`](Annot::Pfoot) annotation.
#[derive(Clone, Debug, Default)]
pub struct MarginStrings {
    pub left: Option<Quoted>,
    pub center: Option<Quoted>,
    pub right: Option<Quoted>,
}

struct Displayable<T>(T);

impl<'a> Display for Displayable<&'a MarginStrings> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> core::fmt::Result {
        todo!()
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

impl Display for Displayable<Point> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let Self(Point { x, y }) = *self;
        write!(f, "{x} {y}")
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Highlight {
    pub color: Color,
    pub opacity: u32,
}

impl Display for Displayable<Highlight> {
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
        Self::None // XXX
    }
}

impl Display for Displayable<Border> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self.0 {
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

#[derive(Debug)]
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
                write!(f, "(rect {} {width} {height})", Displayable(origin))?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
                if let Some(highlight) = highlight {
                    write!(f, "{}", Displayable(highlight))?;
                }
            }
            Self::Oval { origin, width, height, border_always_visible } => {
                write!(f, "(oval {} {width} {height})", Displayable(origin))?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
            }
            Self::Text { origin, width, height, background_color, text_color, pushpin } => {
                write!(f, "(text {} {width} {height})", Displayable(origin))?;
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
                    write!(f, " {}", Displayable(point))?;
                }
                write!(f, ")")?;
                if border_always_visible {
                    write!(f, " (border_avis)")?;
                }
            }
            Self::Line { endpoints: [start, end], arrow, width, color } => {
                write!(f, "(line {} {})", Displayable(start), Displayable(end))?;
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

#[derive(Debug)]
pub struct Maparea {
    pub link: Link,
    pub comment: Quoted,
    pub shape: Shape,
    pub border: Border,
}

impl Display for Maparea {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let Self { ref link, ref comment, ref shape, border } = *self;
        write!(f, "(maparea {link} {comment} {shape} {})", Displayable(border))
    }
}

#[derive(Debug)]
pub enum Annot {
    Background(Color),
    Zoom(Zoom),
    Mode(Mode),
    Align { horiz: HorizAlign, vert: VertAlign },
    Maparea(Maparea),
    Phead(MarginStrings),
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
            Self::Maparea(ref maparea) => write!(f, "{maparea}"),
            Self::Phead(ref strings) => write!(f, "(phead {})", Displayable(strings)),
            Self::Pfoot(ref strings) => write!(f, "(pfoot {})", Displayable(strings)),
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
