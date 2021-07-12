use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
#[serde(rename_all = "snake_case")]
pub struct TypeId(u64);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Name(String);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DataType {
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
