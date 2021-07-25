use parsem::Parsable;
use serde::{Deserialize, Serialize};

use crate::{
    midend::{CompileError, CompileErrorType},
    syntax::{LiteralInteger, TypeRef},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
#[serde(rename_all = "snake_case")]
pub struct TypeId(u64);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(String);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DataType {
    /// Any type (mainly used by corelib builtins)
    Any,
    /// Infer the type if possible
    Infer,
    /// Unreachable
    Never,
    /// Unit type
    Unit,
    /// Truth value
    Boolean,
    /// An unsigned integer of given size
    Integer { bits: usize },
    /// Pointer to any type
    Pointer { to: Box<DataType> },
    /// Function
    Function {
        arguments: Vec<DataType>,
        returns: Box<DataType>,
    },
}
impl DataType {
    pub fn from_type_ref(tr: &TypeRef) -> Result<Self, CompileError> {
        let argc = tr.arguments.as_ref().map(|a| a.items.len()).unwrap_or(0);
        match tr.name.token.text.as_ref() {
            "Never" => {
                if argc == 0 {
                    Ok(Self::Never)
                } else {
                    Err(CompileError {
                        type_: CompileErrorType::TypeTakesNoArguments,
                        tokens: tr.tokens(),
                    })
                }
            }
            "Unit" => {
                if argc == 0 {
                    Ok(Self::Unit)
                } else {
                    Err(CompileError {
                        type_: CompileErrorType::TypeTakesNoArguments,
                        tokens: tr.tokens(),
                    })
                }
            }
            "Boolean" => {
                if argc == 0 {
                    Ok(Self::Boolean)
                } else {
                    Err(CompileError {
                        type_: CompileErrorType::TypeTakesNoArguments,
                        tokens: tr.tokens(),
                    })
                }
            }
            "Integer" => {
                if argc == 0 {
                    Ok(Self::Integer { bits: 64 })
                } else {
                    Err(CompileError {
                        type_: CompileErrorType::TypeTakesNoArguments,
                        tokens: tr.tokens(),
                    })
                }
            }
            other => todo!("Error"),
        }
    }
}

/// A value as it exists compile time
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Value {
    Unit,
    Boolean(bool),
    /// An unsigned integer of given size
    /// TODO: bigint? or size limit on type?
    Integer(u128),
    Pointer {
        heap_ref: usize,
    },
}
impl Value {
    pub fn from_literal(lit: &LiteralInteger) -> Result<Self, CompileError> {
        match lit.token.text.parse::<u128>() {
            Ok(value) => Ok(Self::Integer(value)),
            Err(err) => Err(CompileError {
                type_: CompileErrorType::InvalidLiteral,
                tokens: vec![lit.token.clone()],
            }),
        }
    }
}
