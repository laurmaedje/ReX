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
use crate::font::{FontContext, MathConstants};
use std::ops::Deref;
use std::fmt;
use std::cmp::{max, min};
use crate::dimensions::*;

// By default this will act as a horizontal box
#[derive(Clone, Debug, Default)]
pub struct Layout {
    pub contents: Vec<LayoutNode>,
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

impl Layout {
    pub fn as_node(self) -> LayoutNode {
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

    pub fn new() -> Layout {
        Layout::default()
    }

    pub fn add_node(&mut self, node: LayoutNode) {
        self.width += node.width;
        self.height = max(self.height, node.height);
        self.depth = min(self.depth, node.depth);
        self.contents.push(node);
    }

    pub fn set_offset(&mut self, offset: Length<Px>) {
        self.offset = offset;
    }

    pub fn finalize(mut self) -> Layout {
        self.depth -= self.offset;
        self.height -= self.offset;
        self
    }

    pub fn centered(mut self, new_width: Length<Px>) -> Layout {
        self.alignment = Alignment::Centered(self.width);
        self.width = new_width;
        self
    }

    fn is_symbol(&self) -> Option<LayoutGlyph> {
        if self.contents.len() != 1 {
            return None;
        }
        self.contents[0].is_symbol()
    }
}

#[derive(Clone)]
pub struct LayoutNode {
    pub node: LayoutVariant,
    pub width: Length<Px>,
    pub height: Length<Px>,
    pub depth: Length<Px>,
}

#[derive(Clone)]
pub enum LayoutVariant {
    HorizontalBox(HorizontalBox),
    VerticalBox(VerticalBox),
    Glyph(LayoutGlyph),
    Color(ColorChange),
    Rule,
    Kern,
}

#[derive(Clone)]
pub struct ColorChange {
    pub color: RGBA,
    pub inner: Vec<LayoutNode>,
}

#[derive(Clone, Default)]
pub struct HorizontalBox {
    pub contents: Vec<LayoutNode>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

#[derive(Clone, Default)]
pub struct VerticalBox {
    pub contents: Vec<LayoutNode>,
    pub offset: Length<Px>,
    pub alignment: Alignment,
}

#[derive(Clone, Copy)]
pub struct LayoutGlyph {
    pub gid: u16,
    pub scale: f64,
    pub offset: Length<Px>,
    pub attachment: Length<Px>,
    pub italics: Length<Px>,
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

impl Deref for HorizontalBox {
    type Target = [LayoutNode];
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl Deref for VerticalBox {
    type Target = [LayoutNode];
    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl fmt::Debug for VerticalBox {
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

impl fmt::Debug for HorizontalBox {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HorizontalBox({:?})", self.contents)
    }
}

impl fmt::Debug for LayoutGlyph {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LayoutGlyph({})", self.gid)
    }
}

impl fmt::Debug for LayoutNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.node {
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

impl LayoutNode {
    /// Center the vertical about the axis.
    /// For now this ignores offsets if already applied,
    /// and will break if there already are offsets.
    fn centered(mut self, axis: Length<Px>) -> LayoutNode {
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

    fn is_symbol(&self) -> Option<LayoutGlyph> {
        match self.node {
            LayoutVariant::Glyph(gly) => Some(gly),
            LayoutVariant::HorizontalBox(ref hb) => is_symbol(&hb.contents),
            LayoutVariant::VerticalBox(ref vb) => is_symbol(&vb.contents),
            LayoutVariant::Color(ref clr) => is_symbol(&clr.inner),
            _ => None,
        }
    }
}

pub fn is_symbol(contents: &[LayoutNode]) -> Option<LayoutGlyph> {
    if contents.len() != 1 {
        return None;
    }

    contents[0].is_symbol()
}

/// Display styles which are used in scaling glyphs.  The associated
/// methods are taken from pg.441 from the TeXBook
#[allow(dead_code)]
#[derive(Serialize, Deserialize)]
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

    fn sup_shift_up(self, config: &LayoutSettings) -> Length<Em> {
        match self {
            Style::Display | Style::Text | Style::Script | Style::ScriptScript => {
                config.constants.superscript_shift_up
            }
            _ => config.constants.superscript_shift_up_cramped
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

#[derive(Clone)]
pub struct Constants {
    pub subscript_shift_down: Length<Em>,
    pub subscript_top_max: Length<Em>,
    pub subscript_baseline_drop_min: Length<Em>,

    pub superscript_baseline_drop_max: Length<Em>,
    pub superscript_bottom_min: Length<Em>,
    pub superscript_shift_up_cramped: Length<Em>,
    pub superscript_shift_up: Length<Em>,
    pub sub_superscript_gap_min: Length<Em>,

    pub upper_limit_baseline_rise_min: Length<Em>,
    pub upper_limit_gap_min: Length<Em>,
    pub lower_limit_gap_min: Length<Em>,
    pub lower_limit_baseline_drop_min: Length<Em>,

    pub fraction_rule_thickness: Length<Em>,
    pub fraction_numerator_display_style_shift_up: Length<Em>,
    pub fraction_denominator_display_style_shift_down: Length<Em>,
    pub fraction_num_display_style_gap_min: Length<Em>,
    pub fraction_denom_display_style_gap_min: Length<Em>,
    pub fraction_numerator_shift_up: Length<Em>,
    pub fraction_denominator_shift_down: Length<Em>,
    pub fraction_numerator_gap_min: Length<Em>,
    pub fraction_denominator_gap_min: Length<Em>,

    pub axis_height: Length<Em>,
    pub accent_base_height: Length<Em>,

    pub delimited_sub_formula_min_height: Length<Em>,
    pub display_operator_min_height: Length<Em>,

    pub radical_display_style_vertical_gap: Length<Em>,
    pub radical_vertical_gap: Length<Em>,
    pub radical_rule_thickness: Length<Em>,
    pub radical_extra_ascender: Length<Em>,

    pub stack_display_style_gap_min: Length<Em>,
    pub stack_top_display_style_shift_up: Length<Em>,
    pub stack_top_shift_up: Length<Em>,
    pub stack_bottom_shift_down: Length<Em>,
    pub stack_gap_min: Length<Em>,

    pub delimiter_factor: f64,
    pub delimiter_short_fall: Length<Em>,
    pub null_delimiter_space: Length<Em>,

    pub script_percent_scale_down: f64,
    pub script_script_percent_scale_down: f64,
}

impl Constants {
    pub fn new(math: &MathConstants, font_size: Scale<Px, Em>, font_units_to_em: Scale<Em, Font>) -> Self {
        let em = |v: f64| -> Length<Em> { Length::new(v, Font) * font_units_to_em };
        let font = |v: f64| Length::new(v, Font);

        Constants {
            subscript_shift_down: em(math.subscript_top_max.value.into()),
            subscript_top_max: em(math.subscript_top_max.value.into()),
            subscript_baseline_drop_min: em(math.subscript_baseline_drop_min.value.into()),
            
            superscript_baseline_drop_max: em(math.superscript_baseline_drop_max.value.into()),
            superscript_bottom_min: em(math.superscript_bottom_min.value.into()),
            superscript_shift_up_cramped: em(math.superscript_shift_up_cramped.value.into()),
            superscript_shift_up: em(math.superscript_shift_up.value.into()),
            sub_superscript_gap_min: em(math.sub_superscript_gap_min.value.into()),

            upper_limit_baseline_rise_min: em(math.upper_limit_baseline_rise_min.value.into()),
            upper_limit_gap_min: em(math.upper_limit_gap_min.value.into()),
            lower_limit_gap_min: em(math.lower_limit_gap_min.value.into()),
            lower_limit_baseline_drop_min: em(math.lower_limit_baseline_drop_min.value.into()),

            fraction_rule_thickness: em(math.fraction_rule_thickness.value.into()),
            fraction_numerator_display_style_shift_up: em(math.fraction_numerator_display_style_shift_up.value.into()),
            fraction_denominator_display_style_shift_down: em(math.fraction_denominator_display_style_shift_down.value.into()),
            fraction_num_display_style_gap_min: em(math.fraction_num_display_style_gap_min.value.into()),
            fraction_denom_display_style_gap_min: em(math.fraction_denom_display_style_gap_min.value.into()),
            fraction_numerator_shift_up: em(math.fraction_numerator_shift_up.value.into()),
            fraction_denominator_shift_down: em(math.fraction_denominator_shift_down.value.into()),
            fraction_numerator_gap_min: em(math.fraction_numerator_gap_min.value.into()),
            fraction_denominator_gap_min: em(math.fraction_denominator_gap_min.value.into()),

            axis_height: em(math.axis_height.value.into()),
            accent_base_height: em(math.accent_base_height.value.into()),

            delimited_sub_formula_min_height: em(math.delimited_sub_formula_min_height.into()),

            display_operator_min_height: em(math.display_operator_min_height.into()),

            radical_display_style_vertical_gap: em(math.radical_display_style_vertical_gap.value.into()),
            radical_vertical_gap: em(math.radical_vertical_gap.value.into()),
            radical_rule_thickness: em(math.radical_rule_thickness.value.into()),
            radical_extra_ascender: em(math.radical_extra_ascender.value.into()),

            stack_display_style_gap_min: em(math.stack_display_style_gap_min.value.into()),
            stack_top_display_style_shift_up: em(math.stack_top_display_style_shift_up.value.into()),
            stack_top_shift_up: em(math.stack_top_shift_up.value.into()),
            stack_bottom_shift_down: em(math.stack_bottom_shift_down.value.into()),
            stack_gap_min: em(math.stack_gap_min.value.into()),

            delimiter_factor: 0.901,
            delimiter_short_fall: Length::new(0.1, Em),
            null_delimiter_space: Length::new(0.1, Em),

            script_percent_scale_down: 0.01 * math.script_percent_scale_down.into(),
            script_script_percent_scale_down: 0.01 * math.script_script_percent_scale_down.into(),
        }
    }
}

#[derive(Clone)]
pub struct LayoutSettings<'a> {
    pub ctx: FontContext<'a>,
    pub constants: Constants,
    pub font_size: Scale<Px, Em>,
    pub style: Style,
    pub units_per_em: Scale<Font, Em>,
    pub font_units_to_world: Scale<Px, Font>,
}

impl<'a> LayoutSettings<'a> {
    pub fn new(ctx: FontContext<'a>, font_size: Scale<Px, Em>, style: Style) -> Self {
        use font::Font;
        let font_units_to_em = Scale::new(ctx.font.font_matrix().matrix.m11() as f64, Em, Font);
        let units_per_em = font_units_to_em.inv();
        let constants = Constants::new(&ctx.math.constants, font_size, font_units_to_em);
        LayoutSettings {
            ctx,
            constants,
            font_size,
            style,
            units_per_em,
            font_units_to_world: font_size / units_per_em
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