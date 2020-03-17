pub mod kerning;
mod style;
//mod unit;

use unicode_math::SYMBOLS;
pub use unicode_math::AtomType;
pub use style::style_symbol;

use font::{OpenTypeFont, GlyphId, MathHeader};
pub use font::math::{
    assembly::{Direction, VariantGlyph},
    MathConstants
};
use pathfinder_content::outline::Outline;

use crate::dimensions::{*};

#[derive(Clone)]
pub struct FontContext<'a> {
    pub font: &'a OpenTypeFont<Outline>,
    pub math: &'a MathHeader,
}
impl<'a> FontContext<'a> {
    pub fn glyph(&self, codepoint: char) -> Glyph {
        unimplemented!()
    }
    pub fn glyph_from_gid(&self, gid: u16) -> Glyph {
        unimplemented!()
    }
}

pub struct Glyph<'a> {
    pub ctx: &'a FontContext<'a>,
    pub gid: u16,
    // x_min, y_min, x_max, y_max
    pub bbox: (Length<Font>, Length<Font>, Length<Font>, Length<Font>),
    pub advance: Length<Font>,
    pub lsb: Length<Font>,
    pub italics: Length<Font>,
    pub attachment: Length<Font>,
}
impl<'a> Glyph<'a> {
    pub fn new(font: &'a OpenTypeFont<Outline>, gid: u16) -> Glyph<'a> {
        unimplemented!()
    }
    pub fn height(&self) -> Length<Font> {
        self.bbox.3
    }
    pub fn depth(&self) -> Length<Font> {
        self.bbox.1
    }
    pub fn vert_variant(&self, height: Length<Font>) -> Glyph {
        unimplemented!()
    }
    pub fn horz_variant(&self, width: Length<Font>) -> VariantGlyph {
        unimplemented!()
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Style {
    pub family: Family,
    pub weight: Weight,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub codepoint: char,
    pub atom_type: AtomType
}
impl Symbol {
    pub fn from_name(name: &str) -> Option<Self> {
        SYMBOLS.iter().find(|sym| sym.name == name).map(|sym| {
            Symbol {
                codepoint: sym.codepoint,
                atom_type: sym.atom_type
            }
        })
    }
}

impl Style {
    pub fn new() -> Style {
        Style::default()
    }

    pub fn with_family(self, fam: Family) -> Style {
        Style {
            family: fam,
            ..self
        }
    }

    pub fn with_weight(self, weight: Weight) -> Style {
        Style {
            weight: weight,
            ..self
        }
    }

    pub fn with_bold(self) -> Style {
        Style {
            weight: self.weight.with_bold(),
            ..self
        }
    }

    pub fn with_italics(self) -> Style {
        Style {
            weight: self.weight.with_italics(),
            ..self
        }
    }
}

// NB: Changing the order of these variants requires
//     changing the LUP in fontselection
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Family {
    Roman,
    Script,
    Fraktur,
    SansSerif,
    Blackboard,
    Monospace,
    Normal,
}

// NB: Changing the order of these variants requires
//     changing the LUP in fontselection
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Weight {
    None,
    Italic,
    Bold,
    BoldItalic,
}

impl Weight {
    fn with_bold(self) -> Self {
        match self {
            Weight::Italic | Weight::BoldItalic => Weight::BoldItalic,
            _ => Weight::Bold,
        }
    }

    fn with_italics(self) -> Self {
        match self {
            Weight::Bold | Weight::BoldItalic => Weight::BoldItalic,
            _ => Weight::Italic,
        }
    }
}

impl Default for Family {
    fn default() -> Family {
        Family::Normal
    }
}

impl Default for Weight {
    fn default() -> Weight {
        Weight::None
    }
}
