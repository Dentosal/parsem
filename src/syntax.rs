use regex::Regex;
use strum::EnumDiscriminants;

use parsem;

#[derive(Debug, Clone, PartialEq, Eq, parsem::Scan, parsem::Node)]
#[scan(regex r"#.*\n")]
struct CommentToken(String);

#[derive(Debug, Clone, PartialEq, Eq, parsem::Scan, parsem::Node)]
#[scan(regex r"[a-z][a-zA-Z0-9_]*")]
struct ValueName(String);

#[derive(Debug, Clone, PartialEq, Eq, parsem::Scan, parsem::Node)]
#[scan(regex r"[A-Z][a-zA-Z0-9_]*")]
struct TypeName(String);

#[derive(Debug, Clone, PartialEq, Eq, parsem::Scan, parsem::Node)]
#[scan(regex_capture r"@([A-Z][a-zA-Z0-9_]*)")]
struct GenericArgument(String);

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants, parsem::Scan)]
#[strum_discriminants(name(TokenTextType))]
pub enum TokenText {
    #[scan(regex r"\s+")]
    Whitespace,
    #[scan(regex r"//.*\n")]
    TestThing(String),
    Comment(CommentToken),
    ValueName(ValueName),
    TypeName(TypeName),
    GenericArgument(GenericArgument),
    #[scan(fixed "(")]
    ParenOpen,
    #[scan(fixed ")")]
    ParenClose,
    #[scan(fixed "[")]
    SquareOpen,
    #[scan(fixed "}")]
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

// // #[parsem::alternatives]
// enum TypeRef {
//     /// Either alias or newtype
//     TypeName(TypeName),
//     /// Either alias or newtype
//     Function(Name),
// }

// #[derive(parsem::Node)]
// #[parsem::sequence(TokenTextType::TypeName name, TokenText::Colon, TypeRef type_)]
// struct TypeAlias {
//     name: Token,
//     type_: TypeRef,
// }
