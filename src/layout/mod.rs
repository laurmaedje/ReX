//! This module is where we convert ParseNodes to Layout boxes which are ready to be rendered.
//! The layout boxes follow a similar model as those found in HTML and TeX in that they both
//! have horizontal and vertical boxes.  One difference will be how glue is handled.  HTML/CSS
//! does not have anything similar to how glue is handled in TeX and so aboslute size will be
//! necessary for these scnarios.  It's unclear if we will be able to induce alignments from
//! glue, such as something being centered, aligned left/right, etc.  These questions may
//! also be useful to answer in SVG.
//!
//! Layout boxes will contain a minimal representation of what will be rendered.
//! This includes the container types: Horizontal/Vertical boxes,
//! and primitive types: Symbols, lines, spacing.
//!
//! While rendering in mathmode, most types require an atomtype to determine the kerning
//! between symbols.  This information must also be present with layout boxes.
//!
//! The units used in layout boxes must be in FontUnit (as defined in CSS).

#[macro_use]
mod builders;
mod convert;
pub mod engine;
pub mod spacing;

use crate::parser::color::RGBA;
use crate::font::{FontContext, MathHeader};
use std::ops::Deref;
use std::fmt;
use std::cmp::{max, min};
use std::collections::BTreeMap;
use crate::dimensions::*;

// By default this will act as a horizontal box
#[derive(Clone, Debug, Default)]
pub struct Layout<'f> {
    pub contents: Vec<LayoutNode<'f>>,
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

impl<'f> Layout<'f> {
    pub fn as_node(self) -> LayoutNode<'f> {
        LayoutNode {
            width: self.width,
            height: self.height,
            depth: self.depth,
            node: LayoutVariant::HorizontalBox(HorizontalBox {
                                                   contents: self.contents,
                                                   offset: self.offset,
                                                   alignment: self.alignment,
                                               }),
        }
    }

    pub fn new() -> Layout<'f> {
        Layout::default()
    }

    pub fn add_node(&mut self, node: LayoutNode<'f>) {
        self.width += node.width;
        self.height = max(self.height, node.height);
        self.depth = min(self.depth, node.depth);
        self.contents.push(node);
    }

    pub fn set_offset(&mut self, offset: Length<Px>) {
        self.offset = offset;
    }

    pub fn finalize(mut self) -> Layout<'f> {
        self.depth -= self.offset;
        self.height -= self.offset;
        self
    }

    pub fn centered(mut self, new_width: Length<Px>) -> Layout<'f> {
        self.alignment = Alignment::Centered(self.width);
        self.width = new_width;
        self
    }

    fn is_symbol(&self) -> Option<LayoutGlyph<'f>> {
        if self.contents.len() != 1 {
            return None;
        }
        self.contents[0].is_symbol()
    }
}

#[derive(Clone)]
pub struct LayoutNode<'f> {
    pub node: LayoutVariant<'f>,
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
}

#[derive(Clone)]
pub enum LayoutVariant<'f> {
    Grid(Grid<'f>),
    HorizontalBox(HorizontalBox<'f>),
    VerticalBox(VerticalBox<'f>),
    Glyph(LayoutGlyph<'f>),
    Color(ColorChange<'f>),
    Rule,
    Kern,
}

#[derive(Clone)]
pub struct ColorChange<'f> {
    pub color: RGBA,
    pub inner: Vec<LayoutNode<'f>>,
}

#[derive(Clone)]
pub struct Grid<'f> {
    pub contents: BTreeMap<(usize, usize), LayoutNode<'f>>,
    /// max length of each column
    pub columns: Vec<Length<Px>>,
    /// (max height, max depth) of each row
    pub rows: Vec<(Length<Px>, Length<Px>)>,
}

#[derive(Clone, Default)]
pub struct HorizontalBox<'f> {
    pub contents: Vec<LayoutNode<'f>>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

#[derive(Clone, Default)]
pub struct VerticalBox<'f> {
    pub contents: Vec<LayoutNode<'f>>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

#[derive(Clone, Copy)]
pub struct LayoutGlyph<'f> {
    pub gid: u16,
    pub size: Length<Px>,
    pub offset: Length<Px>,
    pub attachment: Length<Px>,
    pub italics: Length<Px>,
    pub math: &'f MathHeader,
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Alignment {
    Centered(Length<Px>),
    Right(Length<Px>),
    Left,
    Inherit,
    Default,
}

impl Default for Alignment {
    fn default() -> Alignment {
        Alignment::Default
    }
}

impl<'f> Deref for HorizontalBox<'f> {
    type Target = [LayoutNode<'f>];
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<'f> Deref for VerticalBox<'f> {
    type Target = [LayoutNode<'f>];
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<'f> fmt::Debug for VerticalBox<'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.offset.is_zero() {
            write!(f, "VerticalBox({:?})", self.contents)
        } else {
            write!(f,
                   "VerticalBox({:?}, offset: {})",
                   self.contents,
                   self.offset)
        }
    }
}

impl<'f> fmt::Debug for HorizontalBox<'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HorizontalBox({:?})", self.contents)
    }
}

impl<'f> fmt::Debug for LayoutGlyph<'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LayoutGlyph({})", self.gid)
    }
}

impl<'f> fmt::Debug for LayoutNode<'f> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.node {
            LayoutVariant::Grid(_) =>  write!(f, "Grid(..)"),
            LayoutVariant::HorizontalBox(ref hb) => write!(f, "HBox({:?})", hb.contents),
            LayoutVariant::VerticalBox(ref vb) => write!(f, "VBox({:?})", vb.contents),
            LayoutVariant::Glyph(ref gly) => write!(f, "Glyph({:?})", gly),
            LayoutVariant::Rule => write!(f, "Rule()"),
            LayoutVariant::Kern => {
                let kern = if self.width.is_zero() {
                    self.height
                } else {
                    self.width
                };

                write!(f, "Kern({:.1})", kern)
            }
            LayoutVariant::Color(ref clr) => write!(f, "Color({:?}, {:?})", clr.color, clr.inner),
        }
    }
}

impl<'f> LayoutNode<'f> {
    /// Center the vertical about the axis.
    /// For now this ignores offsets if already applied,
    /// and will break if there already are offsets.
    fn centered(mut self, axis: Length<Px>) -> LayoutNode<'f> {
        let shift = (self.height + self.depth) * 0.5 - axis;

        match self.node {
            LayoutVariant::VerticalBox(ref mut vb) => {
                vb.offset = shift;
                self.height -= shift;
                self.depth -= shift;
            }

            LayoutVariant::Glyph(_) => return vbox!(offset: shift; self),

            _ => (),
        }

        self
    }

    fn is_symbol(&self) -> Option<LayoutGlyph<'f>> {
        match self.node {
            LayoutVariant::Glyph(gly) => Some(gly),
            LayoutVariant::HorizontalBox(ref hb) => is_symbol(&hb.contents),
            LayoutVariant::VerticalBox(ref vb) => is_symbol(&vb.contents),
            LayoutVariant::Color(ref clr) => is_symbol(&clr.inner),
            _ => None,
        }
    }
}

pub fn is_symbol<'a, 'b: 'a>(contents: &'a [LayoutNode<'b>]) -> Option<LayoutGlyph<'b>> {
    if contents.len() != 1 {
        return None;
    }

    contents[0].is_symbol()
}

/// Display styles which are used in scaling glyphs.  The associated
/// methods are taken from pg.441 from the TeXBook
#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Style {
    ScriptScriptCramped,
    ScriptScript,
    ScriptCramped,
    Script,
    TextCramped,
    Text,
    DisplayCramped,
    Display,
}

impl Default for Style {
    fn default() -> Style {
        Style::Display
    }
}

#[allow(dead_code)]
impl Style {
    fn cramped(self) -> Style {
        match self {
            Style::ScriptScriptCramped |
            Style::ScriptScript => Style::ScriptScriptCramped,
            Style::ScriptCramped | Style::Script => Style::ScriptCramped,
            Style::TextCramped | Style::Text => Style::TextCramped,
            Style::DisplayCramped | Style::Display => Style::DisplayCramped,
        }
    }

    fn superscript_variant(self) -> Style {
        match self {
            Style::Display | Style::Text => Style::Script,
            Style::DisplayCramped | Style::TextCramped => Style::ScriptCramped,
            Style::Script | Style::ScriptScript => Style::ScriptScript,
            Style::ScriptCramped |
            Style::ScriptScriptCramped => Style::ScriptScriptCramped,
        }
    }

    fn subscript_variant(self) -> Style {
        match self {
            Style::Display | Style::Text | Style::DisplayCramped | Style::TextCramped => {
                Style::ScriptCramped
            }
            Style::Script |
            Style::ScriptScript |
            Style::ScriptCramped |
            Style::ScriptScriptCramped => Style::ScriptScriptCramped,
        }
    }

    fn sup_shift_up(self, config: LayoutSettings) -> Length<Em> {
        match self {
            Style::Display | Style::Text | Style::Script | Style::ScriptScript => {
                config.ctx.constants.superscript_shift_up
            }
            _ => config.ctx.constants.superscript_shift_up_cramped
        }
    }

    fn is_cramped(&self) -> bool {
        match *self {
            Style::Display | Style::Text | Style::Script | Style::ScriptScript => false,
            _ => true,
        }
    }

    fn numerator(self) -> Style {
        match self {
            Style::Display => Style::Text,
            Style::DisplayCramped => Style::TextCramped,
            _ => self.superscript_variant(),
        }
    }

    fn denominator(self) -> Style {
        match self {
            Style::Display | Style::DisplayCramped => Style::TextCramped,
            _ => self.subscript_variant(),
        }
    }
}


#[derive(Copy, Clone)]
pub struct LayoutSettings<'a, 'f> {
    pub ctx: &'a FontContext<'f>,
    pub font_size: Scale<Px, Em>,
    pub style: Style,
}

impl<'a, 'f> LayoutSettings<'a, 'f> {
    pub fn new(ctx: &'a FontContext<'f>, font_size: f64, style: Style) -> Self {
        LayoutSettings {
            ctx,
            font_size: Scale::new(font_size, Px, Em),
            style,
        }
    }

    fn cramped(self) -> Self {
        LayoutSettings {
            style: self.style.cramped(),
            ..self
        }
    }

    fn superscript_variant(self) -> Self {
        LayoutSettings {
            style: self.style.superscript_variant(),
            ..self
        }
    }

    fn subscript_variant(self) -> Self {
        LayoutSettings {
            style: self.style.subscript_variant(),
            ..self
        }
    }

    fn numerator(self) -> Self {
        LayoutSettings {
            style: self.style.numerator(),
            ..self
        }
    }

    fn denominator(self) -> Self {
        LayoutSettings {
            style: self.style.denominator(),
            ..self
        }
    }

    fn with_display(self) -> Self {
        LayoutSettings {
            style: Style::Display,
            ..self
        }
    }

    fn with_text(self) -> Self {
        LayoutSettings {
            style: Style::Text,
            ..self
        }
    }
}
