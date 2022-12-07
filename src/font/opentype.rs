// Stripped down fork of https://github.com/pdf-rs/font
// 
// MIT License
// 
// Copyright (c) 2022 PDF-rs contributors
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![allow(non_snake_case)]

use std::marker::PhantomData;
use std::collections::{HashMap, HashSet};
use itertools::{Itertools, Either};
use std::iter::FromIterator;
use std::cmp::{max, min};

use nom::{
    IResult, 
    Err, 
    error::{VerboseError, VerboseErrorKind},
    number::complete::{be_u16, be_i16},
    sequence::tuple,
};

macro_rules! error {
    ($($t:tt)*) => {
        return Err(FontError::Other(format!($($t)*)))
    };
}

macro_rules! slice {
    ($s:expr, $range:expr) => {
        match $s.get($range) {
            Some(v) => v,
            None => return Err(FontError::OutOfBounds(file!(), line!()))
        }
    };
}

macro_rules! parser {
    ($name:ident : $fun:ident -> $out:ty) => (
        #[allow(non_camel_case_types)]
        pub struct $name;
        impl NomParser for $name {
            type Output = $out;
            fn parse2(data: &[u8])-> ParseResult<Self::Output> {
                $fun(data).map_err(FontError::from)
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = std::mem::size_of::<$out>();
        }
    )
}

macro_rules! parse_field {
    ($start:expr, $input:expr, ?$ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as NomParser>::parse2($input)?;
        if offset != 0 {
            let data = &$start[offset as usize ..];
            let value = <$parser as Parser>::parse(data)?;
            (i, value)
        } else {
            (i, Default::default())
        }
    });
    ($start:expr, $input:expr, @ $ptr:ident $parser:ident, $field:expr) => ({
        let (i, offset) = <$ptr as NomParser>::parse2($input)?;
        assert_ne!(offset, 0, stringify!($field));

        let data = slice!($start, offset as usize ..);
        let value = <$parser as Parser>::parse(data)?;
        (i, value)
    });
    ($start:expr, $input:expr, $parser:ident, $field:expr) => (
        <$parser as NomParser>::parse2($input)?
    );
}
macro_rules! field_size {
    (@ $ptr:ident $(?)* $parser:ident) => (<$ptr as FixedSize>::SIZE);
    ($parser:ident) => (<$parser as FixedSize>::SIZE);
}

macro_rules! table {
    ($name:ident { $( $(#[$meta:meta])* $(?$ptr_opt:ident)* $(@$ptr:ident)* $parser:ident $field:tt, )* } ) => (
        #[derive(Clone, Debug)]
        pub struct $name {
            $(
                $(#[$meta])*
                pub $field: <$parser as Parser>::Output,
            )*
        }
        impl NomParser for $name {
            type Output = $name;
            fn parse2(input: &[u8]) -> ParseResult<$name> {
                let i = input;
                $(
                    let (i, $field) = parse_field!(input, i, $(?$ptr_opt)* $(@$ptr)* $parser, $field);
                )*
                Ok((i, $name { $( $field, )* }))
            }
        }
        impl FixedSize for $name {
            const SIZE: usize = 0 $(+ field_size!($(@$ptr_opt)* $(@$ptr)* $parser) )*;
        }
    );
}


pub type R<'a, T> = IResult<&'a [u8], T, VerboseError<&'a [u8]>>;
pub type ParseResult<'a, T> = Result<(&'a [u8], T), FontError>;

pub struct ParserIterator<'a, T, F> {
    parser: F,
    input: &'a [u8],
    _m: PhantomData<T>
}
impl<'a, T, F: Clone> Clone for ParserIterator<'a, T, F> {
    fn clone(&self) -> Self {
        ParserIterator {
            parser: self.parser.clone(),
            .. *self
        }
    }
}

impl<'a, T, F> Iterator for ParserIterator<'a, T, F> where
    F: Fn(&'a [u8]) -> R<'a, T>
{
    type Item = T;
    #[inline(always)]
    fn next(&mut self) -> Option<T> {
        match (self.parser)(self.input) {
            Ok((i, t)) => {
                self.input = i;
                Some(t)
            }
            Err(_) => None
        }
    }
}
pub fn iterator_n<'a, T, F>(input: &'a [u8], parser: F, n: impl Into<usize>) -> impl Iterator<Item=T> + 'a where
    F: 'a + Fn(&'a [u8]) -> R<'a, T>, T: 'a
{
    ParserIterator { parser, input, _m: PhantomData }.take(n.into())
}


pub struct Offset(pub u16);

impl NomParser for Offset {
    type Output = Self;
    fn parse2(i: &[u8]) -> ParseResult<Self::Output> {
        let (i, offset) = be_u16(i)?;
        Ok((i, Offset(offset)))
    }
}
impl FixedSize for Offset {
    const SIZE: usize = 2;
}
pub fn array_iter<'a, P: Parser + FixedSize>(input: &'a [u8], count: usize)
-> ParseResult<'a, impl Iterator<Item=Result<P::Output, FontError>> + ExactSizeIterator + 'a> {
    let (ours, remaining) = input.split_at(P::SIZE * count);
    let iter = ours.chunks(P::SIZE).map(|chunk| P::parse(chunk));
    Ok((remaining, iter))
}

pub trait Array: Sized {
    type Item;
    type Iter: Iterator<Item=Result<Self::Item, FontError>> + ExactSizeIterator;

    fn len(&self) -> usize;
    fn get(&self, idx: usize) -> Result<Self::Item, FontError>;
    fn into_iter(self) -> Self::Iter;
    fn map<F>(self, f: F) -> ArrayMap<Self, F> {
        ArrayMap { base: self, map: f }
    }
}

#[derive(Clone)]
pub struct ArrayBase<'a, P> {
    data: &'a [u8],
    len: usize,
    _m: PhantomData<P>,
}
pub struct ArrayBaseIter<'a, P: Parser> {
    data: &'a [u8],
    _m: PhantomData<P>,
}
impl<'a, P: Parser + FixedSize> Iterator for ArrayBaseIter<'a, P> {
    type Item = Result<P::Output, FontError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.len() < P::SIZE {
            return None;
        }
        let (this, remaining) = self.data.split_at(P::SIZE);
        self.data = remaining;
        Some(P::parse(this))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.data.len() / P::SIZE;
        (n, Some(n))
    }
}
impl<'a, P: Parser + FixedSize> ExactSizeIterator for ArrayBaseIter<'a, P> {}

impl<'a, P: Parser + FixedSize> Array for ArrayBase<'a, P> {
    type Item = P::Output;
    type Iter = ArrayBaseIter<'a, P>; //impl Iterator<Item=Result<Self::Item, FontError>> + ExactSizeIterator;

    fn len(&self) -> usize {
        self.len
    }
    fn get(&self, idx: usize) -> Result<Self::Item, FontError> {
        let off = P::SIZE * idx;
        P::parse(slice!(self.data, off .. off + P::SIZE))
    }
    fn into_iter(self) -> Self::Iter {
        ArrayBaseIter { data: self.data, _m: PhantomData }
        //self.data.chunks_exact(P::SIZE).map(|chunk| P::parse(chunk))
    }
}
impl<'a, P: Parser + FixedSize> KnownSize for ArrayBase<'a, P> {
    fn size(&self) -> usize {
        self.len * P::SIZE
    }
}
pub struct ArrayMap<A, F> {
    base: A,
    map: F,
}
impl<A: Array, F, T> Array for ArrayMap<A, F> where F: Fn(A::Item) -> Result<T, FontError>
{
    type Item = T;
    type Iter = ArrayMapIter<A, F>;

    fn len(&self) -> usize {
        self.base.len()
    }

    fn get(&self, idx: usize) -> Result<Self::Item, FontError> {
        self.base.get(idx).and_then(|v| (self.map)(v))
    }
    fn into_iter(self) -> Self::Iter {
        ArrayMapIter { base: self.base.into_iter(), map: self.map }
    }
}
impl<A: FixedSize, F> KnownSize for ArrayMap<A, F> {
    fn size(&self) -> usize {
        self.base.size()
    }
}
pub struct ArrayMapIter<A: Array, F> {
    base: A::Iter,
    map: F,
}
impl<A: Array, F, T> Iterator for ArrayMapIter<A, F> where F: Fn(A::Item) -> Result<T, FontError> {
    type Item = Result<T, FontError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.base.next().map(|r| r.and_then(|v| (self.map)(v)))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.base.size_hint()
    }
}
impl<A: Array, F, T> ExactSizeIterator for ArrayMapIter<A, F> where F: Fn(A::Item) -> Result<T, FontError> {}


pub trait Parser {
    type Output;
    fn parse(data: &[u8]) -> Result<Self::Output, FontError>;
}
pub trait NomParser {
    type Output;
    fn parse2(i: &[u8]) -> ParseResult<Self::Output>;
}
pub trait FixedSize {
    const SIZE: usize;
}
pub trait KnownSize {
    fn size(&self) -> usize;
}
impl<T: FixedSize> KnownSize for T {
    fn size(&self) -> usize {
        Self::SIZE
    }
}
impl<P: NomParser> Parser for P {
    type Output = <P as NomParser>::Output;
    fn parse(data: &[u8]) -> Result<Self::Output, FontError> {
        P::parse2(data).map(|(_, v)| v)
    }
}

#[derive(Debug)]
pub enum FontError {
    ParseCharString,
    NoSuchSlot,
    Parse(Err<Vec<VerboseErrorKind>>),
    UnknownMagic([u8; 4]),
    Other(String),
    TypeError(&'static str),
    OutOfBounds(&'static str, u32),
    Get(&'static str, u32, &'static str),
    Require(&'static str),
    Reserved(&'static str, u32, u8),
    Key(&'static str, u32, u8),
    Context(&'static str, u32, Box<FontError>)
}

impl<'a> From<Err<VerboseError<&'a [u8]>>> for FontError {
    fn from(e: Err<VerboseError<&'a [u8]>>) -> Self {
        FontError::Parse(e.map(|e| e.errors.into_iter().map(|(_, k)| k).collect()))
    }
}

parser!(int16 : be_i16 -> i16);
parser!(uint16 : be_u16 -> u16);


impl MathHeader {
    pub fn parse(data: &[u8])-> Result<MathHeader, FontError>  {
        <Self as Parser>::parse(data)
    }
}

pub fn coverage_table<'a>(i: &'a [u8]) -> Result<impl Iterator<Item=u16> + 'a, FontError> {
    let (i, format) = be_u16(i)?;
    match format {
        1 => {
            let (i, glyph_count) = be_u16(i)?;
            Ok(Either::Left(iterator_n(i, be_u16, glyph_count)))
        },
        2 => {
            let (i, range_count) = be_u16(i)?;
            Ok(Either::Right(
                iterator_n(i, tuple((be_u16, be_u16, be_u16)), range_count)
                    .flat_map(|(start, end, _i)| start ..= end)
            ))
        },
        n => error!("invalid coverage format {}", n)
    }
}

#[derive(Default, Clone, Debug)]
pub struct MathValueRecord {
    pub value: i16
}
impl NomParser for MathValueRecord {
    type Output = Self;
    fn parse2(i: &[u8])-> ParseResult<Self::Output> {
        let (i, value) = be_i16(i)?;
        let (i, _offset) = be_i16(i)?;
        Ok((i, MathValueRecord { value }))
    }
}
impl FixedSize for MathValueRecord {
    const SIZE: usize = 4;
}

table!(MathHeader {
    /// Major version of the MATH table, = 1.
    uint16 majorVersion,

    /// Minor version of the MATH table, = 0.
    uint16 minorVersion,

    /// Offset to MathConstants table - from the beginning of MATH table.
    @uint16 MathConstants constants,

    /// Offset to MathGlyphInfo table - from the beginning of MATH table.
    @uint16 MathGlyphInfo glyph_info,

    /// Offset to MathVariants table - from the beginning of MATH table.
    @uint16 MathVariants variants,
});

table!(MathConstants {
    /// Percentage of scaling down for level 1 superscripts and subscripts. Suggested value: 80%.
    int16 script_percent_scale_down,

    /// Percentage of scaling down for level 2 (scriptScript) superscripts and subscripts. Suggested value: 60%.
    int16 script_script_percent_scale_down,

    /// Minimum height required for a delimited expression (contained within parentheses, etc.) to be treated as a sub-formula. Suggested value: normal line height × 1.5.
    uint16 delimited_sub_formula_min_height,

    /// Minimum height of n-ary operators (such as integral and summation) for formulas in display mode (that is, appearing as standalone page elements, not embedded inline within text).
    uint16 display_operator_min_height,

    /// White space to be left between math formulas to ensure proper line spacing. For example, for applications that treat line gap as a part of line ascender, formulas with ink going above (os2.sTypoAscender + os2.sTypoLineGap - MathLeading) or with ink going below os2.sTypoDescender will result in increasing line height.
    MathValueRecord math_leading,

    /// Axis height of the font. In math typesetting, the term axis refers to a horizontal reference line used for positioning elements in a formula. The math axis is similar to but distinct from the baseline for regular text layout. For example, in a simple equation, a minus symbol or fraction rule would be on the axis, but a string for a variable name would be set on a baseline that is offset from the axis. The axisHeight value determines the amount of that offset.
    MathValueRecord axis_height,

    /// Maximum (ink) height of accent base that does not require raising the accents. Suggested: x‑height of the font (os2.sxHeight) plus any possible overshots
    MathValueRecord accent_base_height,

    /// Maximum (ink) height of accent base that does not require flattening the accents. Suggested: cap height of the font (os2.sCapHeight).
    MathValueRecord flattened_accent_base_height,

    /// The standard shift down applied to subscript elements. Positive for moving in the downward direction. Suggested: os2.ySubscriptYOffset.
    MathValueRecord subscript_shift_down,

    /// Maximum allowed height of the (ink) top of subscripts that does not require moving subscripts further down. Suggested: 4/5 x- height.
    MathValueRecord subscript_top_max,

    /// Minimum allowed drop of the baseline of subscripts relative to the (ink) bottom of the base. Checked for bases that are treated as a box or extended shape. Positive for subscript baseline dropped below the base bottom.
    MathValueRecord subscript_baseline_drop_min,

    /// Standard shift up applied to superscript elements. Suggested: os2.ySuperscriptYOffset.
    MathValueRecord superscript_shift_up,

    /// Standard shift of superscripts relative to the base, in cramped style.
    MathValueRecord superscript_shift_up_cramped,

    /// Minimum allowed height of the (ink) bottom of superscripts that does not require moving subscripts further up. Suggested: ¼ x-height.
    MathValueRecord superscript_bottom_min,

    /// Maximum allowed drop of the baseline of superscripts relative to the (ink) top of the base. Checked for bases that are treated as a box or extended shape. Positive for superscript baseline below the base top.
    MathValueRecord superscript_baseline_drop_max,

    /// Minimum gap between the superscript and subscript ink. Suggested: 4 × default rule thickness.
    MathValueRecord sub_superscript_gap_min,

    /// The maximum level to which the (ink) bottom of superscript can be pushed to increase the gap between superscript and subscript, before subscript starts being moved down. Suggested: 4/5 x-height.
    MathValueRecord superscript_bottom_max_with_subscript,

    /// Extra white space to be added after each subscript and superscript. Suggested: 0.5 pt for a 12 pt font.
    MathValueRecord space_after_script,

    /// Minimum gap between the (ink) bottom of the upper limit, and the (ink) top of the base operator.
    MathValueRecord upper_limit_gap_min,

    /// Minimum distance between baseline of upper limit and (ink) top of the base operator.
    MathValueRecord upper_limit_baseline_rise_min,

    /// Minimum gap between (ink) top of the lower limit, and (ink) bottom of the base operator.
    MathValueRecord lower_limit_gap_min,

    /// Minimum distance between baseline of the lower limit and (ink) bottom of the base operator.
    MathValueRecord lower_limit_baseline_drop_min,

    /// Standard shift up applied to the top element of a stack.
    MathValueRecord stack_top_shift_up,

    /// Standard shift up applied to the top element of a stack in display style.
    MathValueRecord stack_top_display_style_shift_up,

    /// Standard shift down applied to the bottom element of a stack. Positive for moving in the downward direction.
    MathValueRecord stack_bottom_shift_down,

    /// Standard shift down applied to the bottom element of a stack in display style. Positive for moving in the downward direction.
    MathValueRecord stack_bottom_display_style_shift_down,

    /// Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element. Suggested: 3 × default rule thickness.
    MathValueRecord stack_gap_min,

    /// Minimum gap between (ink) bottom of the top element of a stack, and the (ink) top of the bottom element in display style. Suggested: 7 × default rule thickness.
    MathValueRecord stack_display_style_gap_min,

    /// Standard shift up applied to the top element of the stretch stack.
    MathValueRecord stretch_stack_top_shift_up,

    /// Standard shift down applied to the bottom element of the stretch stack. Positive for moving in the downward direction.
    MathValueRecord stretch_stack_bottom_shift_down,

    /// Minimum gap between the ink of the stretched element, and the (ink) bottom of the element above. Suggested: same value as upperLimitGapMin.
    MathValueRecord stretch_stack_gap_above_min,

    /// Minimum gap between the ink of the stretched element, and the (ink) top of the element below. Suggested: same value as lowerLimitGapMin.
    MathValueRecord stretch_stack_gap_below_min,

    /// Standard shift up applied to the numerator.
    MathValueRecord fraction_numerator_shift_up,

    /// Standard shift up applied to the numerator in display style. Suggested: same value as stackTopDisplayStyleShiftUp.
    MathValueRecord fraction_numerator_display_style_shift_up,

    /// Standard shift down applied to the denominator. Positive for moving in the downward direction.
    MathValueRecord fraction_denominator_shift_down,

    /// Standard shift down applied to the denominator in display style. Positive for moving in the downward direction. Suggested: same value as stackBottomDisplayStyleShiftDown.
    MathValueRecord fraction_denominator_display_style_shift_down,

    /// Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_numerator_gap_min,

    /// Minimum tolerated gap between the (ink) bottom of the numerator and the ink of the fraction bar in display style. Suggested: 3 × default rule thickness.
    MathValueRecord fraction_num_display_style_gap_min,

    /// Thickness of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_rule_thickness,

    /// Minimum tolerated gap between the (ink) top of the denominator and the ink of the fraction bar. Suggested: default rule thickness.
    MathValueRecord fraction_denominator_gap_min,

    /// Minimum tolerated gap between the (ink) top of the denominator and the ink of the fraction bar in display style. Suggested: 3 × default rule thickness.
    MathValueRecord fraction_denom_display_style_gap_min,

    /// Horizontal distance between the top and bottom elements of a skewed fraction.
    MathValueRecord skewed_fraction_horizontal_gap,

    /// Vertical distance between the ink of the top and bottom elements of a skewed fraction.
    MathValueRecord skewed_fraction_vertical_gap,

    /// Distance between the overbar and the (ink) top of he base. Suggested: 3 × default rule thickness.
    MathValueRecord overbar_vertical_gap,

    /// Thickness of overbar. Suggested: default rule thickness.
    MathValueRecord overbar_rule_thickness,

    /// Extra white space reserved above the overbar. Suggested: default rule thickness.
    MathValueRecord overbar_extra_ascender,

    /// Distance between underbar and (ink) bottom of the base. Suggested: 3 × default rule thickness.
    MathValueRecord underbar_vertical_gap,

    /// Thickness of underbar. Suggested: default rule thickness.
    MathValueRecord underbar_rule_thickness,

    /// Extra white space reserved below the underbar. Always positive. Suggested: default rule thickness.
    MathValueRecord underbar_extra_descender,

    /// Space between the (ink) top of the expression and the bar over it. Suggested: 1¼ default rule thickness.
    MathValueRecord radical_vertical_gap,

    /// Space between the (ink) top of the expression and the bar over it. Suggested: default rule thickness + ¼ x-height.
    MathValueRecord radical_display_style_vertical_gap,

    /// Thickness of the radical rule. This is the thickness of the rule in designed or constructed radical signs. Suggested: default rule thickness.
    MathValueRecord radical_rule_thickness,

    /// Extra white space reserved above the radical. Suggested: same value as radicalRuleThickness.
    MathValueRecord radical_extra_ascender,

    /// Extra horizontal kern before the degree of a radical, if such is present.
    MathValueRecord radical_kern_before_degree,

    /// Negative kern after the degree of a radical, if such is present. Suggested: −10/18 of em.
    MathValueRecord radical_kern_after_degree,

    /// Height of the bottom of the radical degree, if such is present, in proportion to the ascender of the radical sign. Suggested: 60%.
    int16 radical_degree_bottom_raise_percent,
});

table!(MathGlyphInfo {
    /// Offset to MathItalicsCorrectionInfo table, from the beginning of the MathGlyphInfo table.
    @uint16 MathItalicsCorrectionInfo italics_correction_info,

    /// Offset to MathTopAccentAttachment table, from the beginning of the MathGlyphInfo table.
    @uint16 MathTopAccentAttachment top_accent_attachment,

    /// Offset to ExtendedShapes coverage table, from the beginning of the MathGlyphInfo table. When the glyph to the left or right of a box is an extended shape variant, the (ink) box should be used for vertical positioning purposes, not the default position defined by values in MathConstants table. May be NULL.
    ?uint16 ExtendedShapes extended_shape_coverage,

    /// Offset to MathKernInfo table, from the beginning of the MathGlyphInfo table.
    ?uint16 MathKernInfo kern_info,
});

fn merge2<A, B, T, C>(a: A, b: B) -> Result<C, FontError>
where A: Iterator, B: Iterator<Item=Result<T, FontError>>, C: FromIterator<(A::Item, T)>
{
    a.zip(b).map(|(a, b)| match b {
        Ok(b) => Ok((a, b)),
        Err(e) => Err(e)
    }).try_collect()
}
fn merge2rr<A, B, T, U, C>(a: A, b: B) -> Result<C, FontError>
where A: Iterator<Item=Result<T, FontError>>, B: Iterator<Item=Result<U, FontError>>, C: FromIterator<(T, U)>
{
    a.zip(b).map(|(a, b)| match (a, b) {
        (Ok(a), Ok(b)) => Ok((a, b)),
        (Err(e), _) | (_, Err(e)) => Err(e)
    }).try_collect()
}

#[derive(Clone, Debug)]
pub struct MathItalicsCorrectionInfo {
    map: HashMap<u16, MathValueRecord>
}
impl MathItalicsCorrectionInfo {
    pub fn get(&self, gid: u16) -> Option<&MathValueRecord> {
        self.map.get(&gid)
    }
}
impl NomParser for MathItalicsCorrectionInfo {
    type Output = MathItalicsCorrectionInfo;
    fn parse2(data: &[u8]) -> ParseResult<Self> {
        let (i, italics_correction_coverage_offset) = be_u16(data)?;
        let italics_correction_coverage = coverage_table(slice!(data, italics_correction_coverage_offset as usize ..))?;
        let (i, italics_correction_count) = be_u16(i)?;
        let (i, italics_correction) = array_iter::<MathValueRecord>(i, italics_correction_count as usize)?;
        let map = merge2(italics_correction_coverage, italics_correction)?;
        Ok((i, MathItalicsCorrectionInfo { map }))
    }
}

#[derive(Clone, Debug)]
pub struct MathTopAccentAttachment {
    map: HashMap<u16, MathValueRecord>
}
impl MathTopAccentAttachment {
    pub fn get(&self, gid: u16) -> Option<&MathValueRecord> {
        self.map.get(&gid)
    }
}
impl NomParser for MathTopAccentAttachment {
    type Output = MathTopAccentAttachment;
    fn parse2(data: &[u8]) -> ParseResult<Self> {
        let (i, top_accent_coverage_offset) = be_u16(data)?;
        let top_accent_coverage = coverage_table(slice!(data, top_accent_coverage_offset as usize ..))?;
        let (i, top_accent_attachment_count) = be_u16(i)?;
        let (i, top_accent_attachment) = array_iter::<MathValueRecord>(i, top_accent_attachment_count as usize)?;
        let map = top_accent_coverage.zip(top_accent_attachment).map(|(a, b)| b.map(|b| (a, b))).try_collect()?;
        Ok((i, MathTopAccentAttachment { map }))
    }
}

table!(MathGlyphVariantRecord {
    /// Glyph ID for the variant.
    uint16 variant_glyph,

    /// Advance width/height, in design units, of the variant, in the direction of requested glyph extension.
    uint16 advance_measurement,
});

table!(GlyphPartRecord {
    /// Glyph ID for the part.
    uint16 glyph_id,

    /// Advance width/ height, in design units, of the straight bar connector material at the start of the glyph in the direction of the extension (the left end for horizontal extension, the bottom end for vertical extension).
    uint16 start_connector_length,

    /// Advance width/ height, in design units, of the straight bar connector material at the end of the glyph in the direction of the extension (the right end for horizontal extension, the top end for vertical extension).
    uint16 end_connector_length,

    /// Full advance width/height for this part in the direction of the extension, in design units.
    uint16 full_advance,
    /// Part qualifiers. PartFlags enumeration currently uses only one bit:
    /// 0x0001 fExtender If set, the part can be skipped or repeated.
    /// 0xFFFE Reserved.
    uint16 part_flags,
});

impl GlyphPartRecord {
    #[inline]
    pub fn required(&self) -> bool {
        self.part_flags & 1 == 0
    }
    #[inline]
    pub fn optional(&self) -> bool {
        self.part_flags & 1 != 0
    }
}

#[derive(Clone, Debug)]
pub struct GlyphAssembly {
    pub italics_correction: MathValueRecord,
    pub parts: Vec<GlyphPartRecord>
}
impl NomParser for GlyphAssembly {
    type Output = Self;
    fn parse2(data: &[u8]) -> ParseResult<Self> {
        let (i, italics_correction) = MathValueRecord::parse2(data)?;
        let (i, part_count) = be_u16(i)?;
        let (i, parts) = array_iter::<GlyphPartRecord>(i, part_count as usize)?;

        Ok((i, GlyphAssembly {
            italics_correction,
            parts: parts.try_collect()?
        }))
    }
}

#[derive(Clone, Debug)]
pub struct MathGlyphConstruction {
    pub glyph_assembly: Option<GlyphAssembly>,
    pub variants: Vec<MathGlyphVariantRecord>,
}
impl Parser for MathGlyphConstruction {
    type Output = Self;
    fn parse(data: &[u8]) -> Result<Self, FontError> {
        let (i, glyph_assembly_offset) = be_u16(data)?;
        let glyph_assembly = match glyph_assembly_offset {
            0 => None,
            off => Some(GlyphAssembly::parse(slice!(data, off as usize ..))?)
        };
        
        let (i, variant_count) = be_u16(i)?;
        let (_, variants) = array_iter::<MathGlyphVariantRecord>(i, variant_count as usize)?;
        Ok(MathGlyphConstruction {
            glyph_assembly,
            variants: variants.try_collect()?
        })
    }
}

#[derive(Clone, Debug)]
pub struct MathVariants {
    pub min_connector_overlap: u16,
    pub vert_glyph_construction: HashMap<u16, MathGlyphConstruction>,
    pub horiz_glyph_construction: HashMap<u16, MathGlyphConstruction>,
}
impl Parser for MathVariants {
    type Output = MathVariants;
    fn parse(data: &[u8]) -> Result<Self, FontError> {
        let (i, min_connector_overlap) = be_u16(data)?;
        let (i, vert_glyph_coverage_offset) = be_u16(i)?;
        let (i, horiz_glyph_coverage_offset) = be_u16(i)?;

        let (i, vert_glyph_count) = be_u16(i)?;
        let (i, horiz_glyph_count) = be_u16(i)?;

        let (i, vert_glyph_construction_offsets) = array_iter::<uint16>(i, vert_glyph_count as usize)?;
        let (_, horiz_glyph_construction_offsets) = array_iter::<uint16>(i, horiz_glyph_count as usize)?;

        let vert_glyph_construction = if vert_glyph_coverage_offset != 0 {
            let vert_glyph_coverage = coverage_table(slice!(data, vert_glyph_coverage_offset as usize ..))?;
            let vert_glyph_construction = vert_glyph_construction_offsets.map(|off| MathGlyphConstruction::parse(slice!(data, off? as usize ..)));
            merge2(vert_glyph_coverage, vert_glyph_construction)?
        } else {
            HashMap::new()
        };
        
        let horiz_glyph_construction = if horiz_glyph_coverage_offset != 0 {
            let horiz_glyph_coverage = coverage_table(slice!(data, horiz_glyph_coverage_offset as usize ..))?;
            let horiz_glyph_construction = horiz_glyph_construction_offsets.map(|off| MathGlyphConstruction::parse(slice!(data, off? as usize ..)));
            merge2(horiz_glyph_coverage, horiz_glyph_construction)?
        } else {
            HashMap::new()
        };
        
        Ok(MathVariants {
            min_connector_overlap,
            vert_glyph_construction,
            horiz_glyph_construction
        })
    }
}

#[derive(Default, Debug, Clone)]
pub struct MathKern {
    pub pairs: Vec<(MathValueRecord, MathValueRecord)>,
    pub last: MathValueRecord
}
impl Parser for MathKern {
    type Output = Self;
    fn parse(i: &[u8]) -> Result<Self, FontError> {
        let (i, height_count) = be_u16(i)?;
        let (i, heights) = array_iter::<MathValueRecord>(i, height_count as usize)?;
        let (i, kerns) = array_iter::<MathValueRecord>(i, height_count as usize)?;
        let last = MathValueRecord::parse(i)?;
        let pairs = merge2rr(heights, kerns)?;
        Ok(MathKern { pairs, last })
    }
}

impl MathKern {
    pub fn kern_for_height(&self, height: i16) -> i16 {
        for (h, k) in self.pairs.iter() {
            if height < h.value {
                return k.value;
            }
        }
        self.last.value
    }
}

#[derive(Debug, Clone)]
pub struct MathKernInfoRecord {
    pub top_right: MathKern,
    pub top_left: MathKern,
    pub bottom_right: MathKern,
    pub bottom_left: MathKern,
}

#[derive(Clone, Debug, Default)]
pub struct MathKernInfo {
    pub entries: HashMap<u16, MathKernInfoRecord>
}
impl Parser for MathKernInfo {
    type Output = Self;
    fn parse(data: &[u8]) -> Result<Self, FontError> {
        let (i, coverage_offset) = be_u16(data)?;
        let coverage = coverage_table(slice!(data, coverage_offset as usize ..))?;
        let (i, kern_count) = be_u16(i)?;
        let (_, records) = array_iter::<uint16>(i, 4 * kern_count as usize)?;

        let parse_kern = |off| if off > 0 {
            MathKern::parse(slice!(data, off as usize ..))
        } else {
            Ok(MathKern::default())
        };

        let records = records.tuples().map(|(a, b, c, d)| {
            Ok(MathKernInfoRecord {
                top_right: parse_kern(a?)?,
                top_left: parse_kern(b?)?,
                bottom_right: parse_kern(c?)?,
                bottom_left: parse_kern(d?)?,
            })
        });
        let entries = merge2(coverage, records)?;

        Ok(MathKernInfo { entries })
    }
}

#[derive(Clone, Debug, Default)]
pub struct ExtendedShapes {
    pub glyphs: HashSet<u16>
}

impl Parser for ExtendedShapes {
    type Output = Self;
    fn parse(data: &[u8]) -> Result<Self, FontError> {
        let glyphs = coverage_table(data)?;
        Ok(ExtendedShapes { glyphs: glyphs.collect() })
    }
}

#[derive(Debug, Clone)]
pub enum VariantGlyph {
    Replacement(u16),
    Constructable(Direction, Vec<GlyphInstruction>),
}

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Horizontal,
    Vertical
}

#[derive(Debug, Clone, Copy)]
pub struct GlyphInstruction {
    pub gid: u16,
    pub overlap: u16,
}

type FontUnit = u16;

impl MathVariants {
    pub fn vert_variant(&self, glyph: u16, size: u32) -> VariantGlyph {
        let construction = match self.vert_glyph_construction.get(&glyph) {
            Some(construction) => construction,
            None => return VariantGlyph::Replacement(glyph)
        };
        
        // Check if any replacement glyphs meet the requirement.
        for record in &construction.variants {
            if record.advance_measurement as u32 >= size {
                return VariantGlyph::Replacement(record.variant_glyph);
            }
        }

        // Otherwise we check for constructable glyphs.
        // In the scenario that none of the replacement glyphs match the desired
        // advance, and there is no constructable glyph, we return the largest
        // replacement glyph.
        let assembly = match construction.glyph_assembly {
            None => {
                return VariantGlyph::Replacement(construction.variants.last().unwrap().variant_glyph);
            },
            Some(ref assembly) => assembly,
        };

        // Calculate the metrics for a variant at least as large as size.
        let (repeats, diff_ratio) = self.smallest_upper_bound(&assembly.parts, size);
        let instructions = self.construct_glyphs(&assembly.parts, repeats, diff_ratio);
        VariantGlyph::Constructable(Direction::Vertical, instructions)
    }

    pub fn horz_variant(&self, glyph: u16, size: u32) -> VariantGlyph {
        let construction = match self.horiz_glyph_construction.get(&glyph) {
            Some(construction) => construction,
            None => return VariantGlyph::Replacement(glyph)
        };

        // if the first replacement is to big, use the current glyph
        if construction.variants[0].advance_measurement as u32 >= size {
            return VariantGlyph::Replacement(glyph);
        }

        // Check for replacement glyphs that meet the desired size first.
        // We want the largest variant that is _smaller_ than the given size.
        if let Some((replacement, _)) = construction.variants.iter().tuples().find(|(_, b)| b.advance_measurement as u32 >= size) {
            return VariantGlyph::Replacement(replacement.variant_glyph);
        }

        if let Some(ref assembly) = construction.glyph_assembly {
            // Calculate the metrics for a variant at least as large as size.
            if let Some((repeats, diff)) = self.greatest_lower_bound(&assembly.parts, size) {
                let instructions = self.construct_glyphs(&assembly.parts, repeats, diff);
                return VariantGlyph::Constructable(Direction::Horizontal, instructions);
            }
        }

        let backup = construction.variants.last().map(|r| r.variant_glyph).unwrap_or(glyph);
        VariantGlyph::Replacement(backup)
   }

    /// This method will look for a successor of a given glyph if there
    /// exits one.  This is how operators like `\int` and `\sum` become
    /// larger while in Display mode.

    pub fn successor(&self, glyph: u16) -> u16 {
        // If there are no variant glyphs, return itself.
        let variants = match self.vert_glyph_construction.get(&glyph) {
            None => return glyph,
            Some(g) => g,
        };

        // First check to see if any of the replacement glyphs meet the requirement.
        // It is assumed that the glyphs are in increasing advance.
        match variants.variants.get(1) {
            Some(succ) => succ.variant_glyph,
            None => glyph,
        }
    }

    fn construct_glyphs(&self, parts: &[GlyphPartRecord], repeats: u16, diff_ratio: f64) -> Vec<GlyphInstruction> {
        // Construct the variant glyph
        let mut prev_connector = 0;
        let mut first = true;

        let mut variants = Vec::with_capacity(repeats as usize + 3);
        for glyph in parts {
            let repeat = if glyph.optional() { repeats } else { 1 };
            for _ in 0..repeat {
                let overlap = if first {
                    first = false;
                    0
                } else {
                    // linear interpolation
                    //  d * max_overlap + (1 - d) * MIN_CONNECTOR_OVERLAP
                    let max = self.max_overlap(prev_connector, glyph);
                    let overlap = (1.0 - diff_ratio) * max as f64
                        + diff_ratio * self.min_connector_overlap as f64;
                    overlap as u16
                };
                prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);

                variants.push(GlyphInstruction {
                    gid: glyph.glyph_id,
                    overlap: overlap
                });
            }
        }

        variants
    }

    /// Construct the smallest variant that is larger than the given size.
    /// With the number of glyphs required to construct the variant is larger
    /// than `ITERATION_LIMIT` we return `None`.
    fn smallest_upper_bound(&self, parts: &[GlyphPartRecord], size: u32) -> (u16, f64) {
        // Otherwise, check the next largest variant with optional glyphs included.
        let (mut small, mut large, opt_small, opt_large) = self.advance_with_optional(parts);
        if large >= size {
            let diff_ratio = f64::from(size - small) / f64::from(large - small);
            return (1, diff_ratio);
        } 

        // We need to find the smallest integer k that satisfies:
        //     large + k * opt_large >= size
        // This is solved by:
        //     (size - large) / opt_large <= k
        // So take k = ceil[ (size - large) / opt_large ]
        let k = u32::from( (size - large) / opt_large ) + 1;
        small += k * opt_small;
        large += k * opt_large;

        //  A---o---B, percentage: (o - A) / (B - A)
        // o  A-----B, percentage: 0 (smallest glyph).
        // Need small + diff_ratio * (opt_large - opt_small) = size
        if small >= size {
            return (k as u16 + 1, 0.into());
        }

        let difference_ratio = f64::from(size - small) / f64::from(large - small);
        (k as u16 + 1, difference_ratio)
    }

    /// Measure the _largest_ a glyph construction _smaller_ than the given size. 
    /// If all constructions are larger than the given size, return `None`.
    /// Otherwise return the number of optional glyphs required and the difference
    /// ratio to obtain the desired size.
    fn greatest_lower_bound(&self,
        parts: &[GlyphPartRecord], 
        size: u32) 
    -> Option<(u16, f64)> {
        let (small, large) = self.advance_without_optional(parts);
        if small >= size {
            return None;
        }

        // Otherwise calculate the size of including one set of optional glyphs.
        let (mut ssmall, mut llarge, opt_small, opt_large) = self.advance_with_optional(parts);

        // If the smallest constructable with optional glyphs is too large we
        // use no optional glyphs.
        // TODO: Do something better if `large == small`.
        if ssmall >= size {
            let diff_ratio = f64::from(size - small) / f64::from(large - small);
            let diff_ratio = diff_ratio.min(1.0);
            return Some((0, diff_ratio));
        }

        // Determine the number of additional optional glyphs required to achieve size.
        // We need to find the smallest integer k such that:
        //     ssmall + k*opt_small >= size
        // This is solved by:
        //     (size - ssmall) / opt_small <= k
        // Which is solved by: k = floor[ (size - smmal) / opt_small ]
        // Since we round towards zero, floor is not necessary.
        let k = (size - ssmall) / opt_small;

        ssmall += k * opt_small;
        llarge += k * opt_large;
        let diff_ratio = f64::from(size - ssmall) / f64::from(llarge - ssmall);
        let diff_ratio = diff_ratio.min(1.0).max(0.0);

        Some((k as u16 + 1, diff_ratio))
    }

    /// Calculate the advance of the smallest variant with exactly one set of optional
    /// connectors. This returns a tuple: the first element states the advance of a
    /// variant with one set of optional connectors, the second element states the
    /// increase in advance for each additional connector.
    fn advance_with_optional(&self, parts: &[GlyphPartRecord]) -> (u32, u32, u32, u32) {
        let mut advance_small = 0;
        let mut advance_large = self.min_connector_overlap as u32;
        let mut connector_small = 0;
        let mut connector_large = 0;
        let mut prev_connector = 0;

        // Calculate the legnth with exactly one connector
        for glyph in parts {
            let overlap = self.max_overlap(prev_connector, glyph);
            advance_small += (glyph.full_advance - overlap) as u32;
            advance_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);

            // Keep record of the advance each additional connector adds
            if glyph.optional() {
                let overlap = self.max_overlap(glyph.start_connector_length, glyph);
                connector_small += (glyph.full_advance - overlap) as u32;
                connector_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            }
        }

        (advance_small, advance_large, connector_small, connector_large)
    }

    fn advance_without_optional(&self, parts: &[GlyphPartRecord]) -> (u32, u32) {
        let mut advance_small = 0;
        let mut advance_large = self.min_connector_overlap as u32;
        let mut prev_connector = 0;

        for glyph in parts.iter().filter(|glyph| glyph.optional()) {
            let overlap = self.max_overlap(prev_connector, glyph);
            advance_small += (glyph.full_advance - overlap) as u32;
            advance_large += (glyph.full_advance - self.min_connector_overlap) as u32;
            prev_connector = min(glyph.end_connector_length, glyph.full_advance / 2);
        }

        (advance_small, advance_large)
    }

    fn max_overlap(&self, left: FontUnit, right: &GlyphPartRecord) -> FontUnit {
        let overlap = min(left, right.start_connector_length);
        let overlap = min(overlap, right.full_advance / 2);
        max(overlap, self.min_connector_overlap)
    }
}
