use regex::Regex;
use strum::EnumDiscriminants;

use parsem;

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants, parsem::Scan)]
#[strum_discriminants(name(TokenTextType))]
pub enum TokenText {
    #[scan(regex r"\s+")]
    Whitespace,
    #[scan(regex r"//.*\n")]
    TestThing(String),
    #[scan(regex r"#.*\n")]
    Comment(String),
    #[scan(regex r"[a-z][a-zA-Z0-9_]*")]
    ValueName(String),
    #[scan(regex r"[A-Z][a-zA-Z0-9_]*")]
    TypeName(String),
    #[scan(regex_capture r"@([A-Z][a-zA-Z0-9_]*)")]
    GenericArgument(String),
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
    #[scan(fixed "=>")]
    Arrow,
    #[scan(fixed ":=")]
    AssignType,
    #[scan(fixed "=")]
    AssignValue,
    #[scan(fixed ":")]
    Colon,
    #[scan(fixed ",")]
    Comma,
}

#[derive(Debug, Clone)]
pub struct Location {
    file: String,
    offset: usize,
    length: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    text: TokenText,
    location: Location,
}
