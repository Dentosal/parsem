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
    /// Generic type parameter
    Generic { id: TypeId },
    /// Global "newtype"
    NewType { id: TypeId, body: Box<DataType> },
    /// Product type / record / struct
    Product {
        /// Set of fields without defined order
        fields: HashMap<Name, DataType>,
    },
    /// Sum type / discriminated union / Rust-style enum
    Sum {
        /// Set of variants
        variants: HashMap<Name, DataType>,
    },
    /// Function
    Function {
        takes: Box<DataType>,
        returns: Box<DataType>,
    },
}
impl DataType {
    pub fn unit() -> Self {
        Self::Product {
            fields: HashMap::new(),
        }
    }

    pub fn never() -> Self {
        Self::Sum {
            variants: HashMap::new(),
        }
    }
}
