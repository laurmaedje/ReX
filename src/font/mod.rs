pub mod kerning;
mod style;
mod opentype;

pub use unicode_math::AtomType;
pub use style::style_symbol;

pub use opentype::{
    MathHeader,
    Direction,
    MathConstants,
    VariantGlyph
};

use crate::dimensions::{*};
use crate::error::FontError;

#[derive(Clone)]
pub struct FontContext<'f> {
    face: &'f ttf_parser::Face<'f>,
    math: &'f MathHeader,
    pub constants: Constants,
    pub units_per_em: Scale<Font, Em>,
}
impl<'f> FontContext<'f> {
    pub fn new(face: &'f ttf_parser::Face<'f>, math: &'f MathHeader) -> Self {
        let units_per_em = Scale::new(face.units_per_em() as f64, Font, Em);
        let font_units_to_em = units_per_em.inv();
        let constants = Constants::new(&math.constants, font_units_to_em);
        FontContext {
            face,
            math,
            units_per_em,
            constants
        }
    }
    pub fn glyph(&self, codepoint: char) -> Result<Glyph<'f>, FontError> {
        let gid = self.face.glyph_index(codepoint).ok_or(FontError::MissingGlyphCodepoint(codepoint))?;
        self.glyph_from_gid(gid.0 as u16)
    }
    pub fn glyph_from_gid(&self, gid: u16) -> Result<Glyph<'f>, FontError> {
        let id = ttf_parser::GlyphId(gid);
        let advance = self.face.glyph_hor_advance(id).ok_or(FontError::MissingGlyphGID(gid))?;
        let lsb = self.face.glyph_hor_side_bearing(id).ok_or(FontError::MissingGlyphGID(gid))?;
        let italics = self.math.glyph_info.italics_correction_info.get(gid).map(|info| info.value).unwrap_or_default();
        let attachment = self.math.glyph_info.top_accent_attachment.get(gid).map(|info| info.value).unwrap_or_default();
        let bbox = self.face.glyph_bounding_box(id).ok_or(FontError::MissingGlyphGID(gid))?;
        Ok(Glyph {
            gid,
            math: self.math,
            advance: Length::new(advance, Font),
            lsb: Length::new(lsb, Font),
            italics: Length::new(italics, Font),
            attachment: Length::new(attachment, Font),
            bbox: (
                Length::new(bbox.x_min, Font),
                Length::new(bbox.y_min, Font),
                Length::new(bbox.x_max, Font),
                Length::new(bbox.y_max, Font),
            )
        })
    }
    pub fn vert_variant(&self, codepoint: char, height: Length<Font>) -> Result<VariantGlyph, FontError> {
        let gid = self.face.glyph_index(codepoint).ok_or(FontError::MissingGlyphCodepoint(codepoint))?;
        Ok(self.math.variants.vert_variant(gid.0, (height / Font) as u32))
    }
    pub fn horz_variant(&self, codepoint: char, width: Length<Font>) -> Result<VariantGlyph, FontError> {
        let gid = self.face.glyph_index(codepoint).ok_or(FontError::MissingGlyphCodepoint(codepoint))?;
        Ok(self.math.variants.horz_variant(gid.0, (width / Font) as u32))
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
    pub fn new(math: &MathConstants, font_units_to_em: Scale<Em, Font>) -> Self {
        let em = |v: f64| -> Length<Em> { Length::new(v, Font) * font_units_to_em };

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

            script_percent_scale_down: 0.01 * math.script_percent_scale_down as f64,
            script_script_percent_scale_down: 0.01 * math.script_script_percent_scale_down as f64,
        }
    }
}

pub struct Glyph<'f> {
    pub math: &'f MathHeader,
    pub gid: u16,
    // x_min, y_min, x_max, y_max
    pub bbox: (Length<Font>, Length<Font>, Length<Font>, Length<Font>),
    pub advance: Length<Font>,
    pub lsb: Length<Font>,
    pub italics: Length<Font>,
    pub attachment: Length<Font>,
}
impl<'f> Glyph<'f> {
    pub fn height(&self) -> Length<Font> {
        self.bbox.3
    }
    pub fn depth(&self) -> Length<Font> {
        self.bbox.1
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Style {
    pub family: Family,
    pub weight: Weight,
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
