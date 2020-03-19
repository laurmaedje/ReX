use crate::lexer::OwnedToken;
use std::error;
use std::fmt;
use crate::font::{AtomType};
use crate::parser::symbols::Symbol;

pub type Result<T> = ::std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    UnrecognizedCommand(String),
    UnrecognizedSymbol(char),
    FailedToParse(OwnedToken),
    ExcessiveSubscripts,
    ExcessiveSuperscripts,
    LimitsMustFollowOperator,
    ExpectedMathField(OwnedToken),
    MissingSymbolAfterDelimiter,
    MissingSymbolAfterAccent,
    ExpectedAtomType(AtomType, AtomType),
    ExpectedSymbol(OwnedToken),
    RequiredMacroArg,
    ExpectedTokenFound(OwnedToken, OwnedToken),
    ExpectedOpen(Symbol),
    ExpectedClose(Symbol),
    ExpectedOpenGroup,
    NoClosingBracket,
    StackMustFollowGroup,
    AccentMissingArg(String),
    UnexpectedEof(OwnedToken),
    UnrecognizedDimension,
    UnrecognizedColor(String),
    MissingGlyphCodepoint(char),
    MissingGlyphGID(u16),
    Todo,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            UnrecognizedCommand(ref cmd) =>
                write!(f, "unrecognized command: \\{}`", cmd),
            UnrecognizedSymbol(c) =>
                write!(f, "unrecognized symbol '{}'", c),
            FailedToParse(ref tok) =>
                write!(f, "failed to parse `{}`", tok),
            ExcessiveSubscripts =>
                write!(f, "an excessive number of subscripts"),
            ExcessiveSuperscripts =>
                write!(f, "excessive number of superscripts"),
            LimitsMustFollowOperator =>
                write!(f, "limit commands must follow an operator"),
            ExpectedMathField(ref field) =>
                write!(f, "expected math field, found `{}`", field),
            MissingSymbolAfterDelimiter =>
                write!(f, "missing symbol following delimiter"),
            MissingSymbolAfterAccent =>
                write!(f, "missing symbol following accent"),
            ExpectedAtomType(left, right) =>
                write!(f, "expected atom type {:?} found {:?}", left, right),
            ExpectedSymbol(ref sym) =>
                write!(f, "expected symbol, found {}", sym),
            RequiredMacroArg =>
                write!(f, "missing required macro argument"),
            ExpectedTokenFound(ref expected, ref found) =>
                write!(f, "expected {} found {}", expected, found),
            ExpectedOpen(sym) =>
                write!(f, "expected Open, Fence, or period after '\\left', found `{:?}`", sym),
            ExpectedClose(sym) =>
                write!(f, "expected Open, Fence, or period after '\\right', found `{:?}`", sym),
            ExpectedOpenGroup =>
                write!(f, "expected an open group symbol"),
            NoClosingBracket =>
                write!(f, "failed to find a closing bracket"),
            StackMustFollowGroup =>
                write!(f, "stack commands must follow a group"),
            AccentMissingArg(ref acc) =>
                write!(f, "the accent '\\{}' must have an argument", acc),
            UnexpectedEof(ref tok) =>
                write!(f, "unexpectedly ended parsing; unmatched end of expression? Stoped parsing at {}", tok),
            UnrecognizedDimension =>
                write!(f, "failed to parse dimension"),
            UnrecognizedColor(ref color) =>
                write!(f, "failed to recognize the color '{}'", color),
            MissingGlyphCodepoint(cp) =>
                write!(f, "missing glyph for codepoint'{}'", cp),
            MissingGlyphGID(gid) =>
                write!(f, "missing glyph with gid {}", gid),
            Todo =>
                write!(f, "failed with an unspecified error that has yet be implemented"),
        }
    }
}