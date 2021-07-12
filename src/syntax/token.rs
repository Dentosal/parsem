use regex::Regex;
use strum::EnumDiscriminants;

use parsem;

pub use parsem::Location;

#[derive(Debug, Clone, PartialEq, Eq, parsem::Scan)]
pub enum TokenType {
    #[scan(regex r"\s+")]
    Whitespace,
    #[scan(regex r"#.*\n")]
    Comment,
    #[scan(regex r"fn\b")]
    KeywordFn,
    #[scan(regex r"return\b")]
    KeywordReturn,
    #[scan(regex r"let\b")]
    KeywordLet,
    #[scan(regex r"if\b")]
    KeywordIf,
    #[scan(regex r"else\b")]
    KeywordElse,
    #[scan(regex r"while\b")]
    KeywordWhile,
    #[scan(regex r"break\b")]
    KeywordBreak,
    #[scan(regex r"continue\b")]
    KeywordContinue,
    #[scan(regex r"[a-z][a-zA-Z0-9_]*")]
    ValueName,
    #[scan(regex r"[A-Z][a-zA-Z0-9_]*")]
    TypeName,
    #[scan(regex r"[0-9]([0-9]*[0-9])?")]
    LiteralInteger,
    #[scan(fixed "(")]
    ParenOpen,
    #[scan(fixed ")")]
    ParenClose,
    #[scan(fixed "[")]
    SquareOpen,
    #[scan(fixed "]")]
    SquareClose,
    #[scan(fixed "{")]
    CurlyOpen,
    #[scan(fixed "}")]
    CurlyClose,
    #[scan(fixed "<")]
    AngleOpen,
    #[scan(fixed ">")]
    AngleClose,
    #[scan(fixed "->")]
    Arrow,
    #[scan(fixed "=")]
    AssignOp,
    #[scan(fixed ";")]
    Semicolon,
    #[scan(fixed ":")]
    Colon,
    #[scan(fixed ",")]
    Comma,
}

pub type Token = parsem::Token<TokenType>;
