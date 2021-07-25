pub mod interpreter;
pub mod ir;

use parsem::Location;

use crate::data::{DataType, Value};
use crate::syntax::{Block, FunctionDefinition, ValueName, WhileStmt};
use crate::syntax::{Token, TypeRef};

use self::ir::{Instruction, LabelId};

#[derive(Debug, Clone)]
pub enum ValueStorage {
    Global { name: String },
    Local { stack_index: usize },
}
impl CompileToIr for ValueStorage {
    fn compile_to_ir(&self, ctx: &mut CompileContext) -> Result<Vec<Instruction>, CompileError> {
        match self.clone() {
            Self::Global { name } => Ok(vec![ir::Instruction::PushRef { name }]),
            Self::Local { stack_index } => Ok(vec![ir::Instruction::Copy {
                depth: ctx.stack_index_to_depth(stack_index),
            }]),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompileContext {
    pub scopes: Vec<CompileContextScope>,
}
impl CompileContext {
    pub fn resolve_name(&self, name: &ValueName) -> Result<ScopedName, CompileError> {
        println!("Resolving {:?}", name.token.text);
        for scope in self.scopes.iter().rev() {
            for scoped in &scope.names {
                println!("Resolve? {:?}", scoped.name);
                if scoped.name == name.token.text {
                    return Ok(scoped.clone());
                }
            }
        }

        println!("Cound not resolve {:?}", name);
        Err(CompileError {
            type_: CompileErrorType::NoSuchName,
            tokens: vec![name.token.clone()],
        })
    }

    pub fn resolve_type(&self, type_: &TypeRef) -> Result<DataType, CompileError> {
        // TODO: type alias resolution
        Ok(DataType::from_type_ref(type_)?)
    }

    /// Convert index from the bottom of the stack to depth in the stack
    pub fn stack_index_to_depth(&self, index: usize) -> usize {
        let full_depth: usize = self.scopes.iter().map(|s| s.stack_size).sum();
        assert!(
            index < full_depth,
            "Cannot convert index {} to depth, limit {}",
            index,
            full_depth
        );
        full_depth - index - 1
    }

    /// Return stack index from the bottom
    pub fn stack_reserve_slot(&mut self) -> usize {
        let depth = self.scopes.iter().map(|s| s.stack_size).sum();
        self.scopes.last_mut().unwrap().stack_size += 1;
        depth
    }
}

#[derive(Debug, Clone)]
pub struct CompileContextScope {
    pub type_: ScopeType,
    pub names: Vec<ScopedName>,
    pub stack_size: usize,
}
impl CompileContextScope {
    pub fn new(type_: ScopeType) -> Self {
        Self {
            type_,
            names: Vec::new(),
            stack_size: 0,
        }
    }

    pub fn prelude() -> Self {
        Self {
            type_: ScopeType::Prelude,
            names: crate::corelib::BUILTINS
                .iter()
                .cloned()
                .map(|(name, type_, _)| ScopedName {
                    name: name.to_owned(),
                    type_,
                    storage: ValueStorage::Global {
                        name: name.to_owned(),
                    },
                })
                .collect(),
            stack_size: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopedName {
    pub name: String,
    pub type_: DataType,
    pub storage: ValueStorage,
}

#[derive(Debug, Clone)]
pub enum ScopeType {
    Prelude,
    Module,
    Function {
        def: FunctionDefinition,
        end_label: LabelId,
    },
    WhileLoop(WhileStmt),
    Block(Block),
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub type_: CompileErrorType,
    pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum CompileErrorType {
    InvalidLiteral,
    NoSuchName,
    TypeTakesNoArguments,
}

pub trait CompileToIr {
    fn compile_to_ir(&self, ctx: &mut CompileContext) -> Result<Vec<Instruction>, CompileError>;
}
