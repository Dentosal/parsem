use regex::Regex;
use strum::EnumDiscriminants;

use parsem;

// mod token;

pub use self::token::{Location, Token, TokenText};

#[derive(Debug, Clone, PartialEq, Eq, EnumDiscriminants)]
#[strum_discriminants(name(NodeType))]
pub enum Node {
    Assign {},
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
