//! This is a collection of tools used for converting ParseNodes into LayoutNodes.

use crate::font::{Glyph, Direction, VariantGlyph};
use crate::dimensions::{*};
use crate::layout::LayoutSettings;

use super::{Style};
use super::builders;
use super::{LayoutNode, LayoutVariant, LayoutGlyph};
use crate::parser::nodes::Rule;

pub trait AsLayoutNode {
    fn as_layout(&self, config: LayoutSettings) -> LayoutNode;
}

impl<'a> AsLayoutNode for Glyph<'a> {
    fn as_layout(&self, config: LayoutSettings) -> LayoutNode {
        LayoutNode {
            height: self.height().scaled(config),
            width:  self.advance.scaled(config),
            depth:  self.depth().scaled(config),
            node:   LayoutVariant::Glyph(LayoutGlyph {
                gid: self.gid,
                scale: config.scale_factor(),
                attachment: self.attachment.scaled(config),
                italics: self.italics.scaled(config),
                offset:  Length::zero(),
            })
        }
    }
}

impl AsLayoutNode for Rule {
    fn as_layout(&self, config: LayoutSettings) -> LayoutNode {
        LayoutNode {
            node:   LayoutVariant::Rule,
            width:  self.width .scaled(config),
            height: self.height.scaled(config),
            depth:  Length::zero(),
        }
    }
}

impl AsLayoutNode for VariantGlyph {
    fn as_layout(&self, config: LayoutSettings) -> LayoutNode {
        match *self {
            VariantGlyph::Replacement(gid) => {
                let glyph = config.ctx.glyph_from_gid(gid);
                glyph.as_layout(config)
            },

            VariantGlyph::Constructable(dir, ref parts) => {
                match dir {
                    Direction::Vertical => {
                        let mut contents = builders::VBox::new();
                        for instr in parts {
                            let glyph = config.ctx.glyph_from_gid(instr.gid);
                            contents.insert_node(0, glyph.as_layout(config));
                            if instr.overlap != 0 {
                                let overlap = Length::new(instr.overlap, Font);
                                let kern = -(overlap + glyph.depth()).scaled(config);
                                contents.add_node(kern!(vert: kern));
                            }
                        }

                        contents.build()
                    },

                    Direction::Horizontal => {
                        let mut contents = builders::HBox::new();
                        for instr in parts {
                            let glyph = config.ctx.glyph_from_gid(instr.gid);
                            if instr.overlap != 0 {
                                let kern = -Length::new(instr.overlap, Font).scaled(config);
                                contents.add_node(kern!(horz: kern));
                            }
                            contents.add_node(glyph.as_layout(config));
                        }

                        contents.build()
                    }
                }
            },
        }
    }
}

impl<'a> LayoutSettings<'a> {
    #[inline]
    fn scale_factor(&self) -> f64 {
        match self.style {
            Style::Display |
            Style::DisplayCramped |
            Style::Text |
            Style::TextCramped
                => 1.0,

            Style::Script |
            Style::ScriptCramped
                => 0.01 * self.ctx.constants.script_percent_scale_down,

            Style::ScriptScript |
            Style::ScriptScriptCramped
                => 0.01 * self.ctx.constants.script_script_percent_scale_down,
        }
    }
    #[inline]
    fn scale_font_unit(&self, length: Length<Font>) -> Length<Px> {
        length / self.ctx.units_per_em * self.font_size
    }
    #[inline]
    fn scale_length<U>(&self, length: Length<U>) -> Length<U> {
        length * self.scale_factor()
    }
    #[inline]
    pub fn to_font(&self, length: Length<Px>) -> Length<Font> {
        length / self.font_size * self.ctx.units_per_em
    }
}
pub trait Scaled {
    fn scaled(self, config: LayoutSettings) -> Length<Px>;
}

impl Scaled for Length<Font> {
    #[inline]
    fn scaled(self, config: LayoutSettings) -> Length<Px> {
        config.scale_font_unit(self)
    }
}

impl Scaled for Length<Px> {
    #[inline]
    fn scaled(self, config: LayoutSettings) -> Length<Px> {
        self
    }
}
impl Scaled for Length<Em> {
    #[inline]
    fn scaled(self, config: LayoutSettings) -> Length<Px> {
        self * config.font_size
    }
}
impl Scaled for Unit {
    #[inline]
    fn scaled(self, config: LayoutSettings) -> Length<Px> {
        match self {
            Unit::Em(em) => Length::new(em, Em) * config.font_size,
            Unit::Px(px) => Length::new(px, Px)
        }
    }
}
