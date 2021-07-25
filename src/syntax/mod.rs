use std::collections::HashMap;

use parsem::{Node, Parsable};

mod token;

use self::ir::LabelId;
pub use self::token::{Location, Token, TokenType};
pub use crate::midend::{ir, CompileContext, CompileError, CompileToIr};
use crate::midend::{CompileContextScope, ScopeType, ValueStorage};
use crate::{
    data::{DataType, Value},
    midend::ScopedName,
};

parsem::token_wrapper!(TokenType;
    KeywordFn, KeywordReturn,
    KeywordLet,
    KeywordIf, KeywordElse,
    KeywordWhile, KeywordBreak, KeywordContinue,
    Arrow, AssignOp,
    ValueName, TypeName,
    LiteralInteger,
    Comma, Colon, Semicolon,
    ParenOpen, ParenClose, CurlyOpen, CurlyClose,
);

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Punctuated<T: Parsable<TokenType>, P: Parsable<TokenType>> {
    values: Option<Vec<PunctuatedValue<T, P>>>,
    /// Last item, in case there is no trailing comma
    last: Option<Box<T>>,
}
impl<T: Parsable<TokenType>, P: Parsable<TokenType>> Punctuated<T, P> {
    pub fn len(&self) -> usize {
        self.values.as_ref().map(|v| v.len()).unwrap_or(0)
            + (if self.last.is_some() { 1 } else { 0 })
    }

    pub fn iter<'a>(&'a self) -> PunctuatedIter<'a, T, P> {
        PunctuatedIter {
            inner: self,
            cursor: 0,
        }
    }
}

pub struct PunctuatedIter<'a, T: Parsable<TokenType>, P: Parsable<TokenType>> {
    inner: &'a Punctuated<T, P>,
    cursor: usize,
}

impl<'a, T: Parsable<TokenType> + Clone, P: Parsable<TokenType>> Iterator
    for PunctuatedIter<'a, T, P>
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.cursor;
        if let Some(vs) = &self.inner.values {
            if index < vs.len() {
                self.cursor += 1;
                Some(vs[index].value.clone())
            } else if index == vs.len() {
                self.cursor += 1;
                self.inner.last.as_ref().map(|v| *v.clone())
            } else {
                None
            }
        } else if index == 0 {
            self.cursor += 1;
            self.inner.last.as_ref().map(|v| *v.clone())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct PunctuatedValue<T: Parsable<TokenType>, P: Parsable<TokenType>> {
    value: T,
    punct: P,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Module {
    pub items: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct FunctionDefinition {
    pub fn_keyword: KeywordFn,
    pub name: ValueName,
    pub parameters: ParameterList,
    pub arrow: Arrow,
    pub return_type: TypeRef,
    pub body: Block,
}

impl CompileToIr for FunctionDefinition {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = Vec::new();

        // TODO: closures

        let parent_scope = ctx.scopes.last_mut().unwrap();
        parent_scope.names.push(ScopedName {
            name: self.name.token.text.clone(),
            type_: DataType::Function {
                arguments: self
                    .parameters
                    .items
                    .iter()
                    .map(|p| DataType::from_type_ref(&p.type_.type_))
                    .collect::<Result<_, _>>()?,
                returns: Box::new(DataType::from_type_ref(&self.return_type)?),
            },
            storage: ValueStorage::Global {
                name: self.name.token.text.clone(),
            },
        });

        let end_label = LabelId::new();

        ctx.scopes
            .push(CompileContextScope::new(ScopeType::Function {
                def: self.clone(),
                end_label,
            }));

        for param in self.parameters.items.iter() {
            let type_ = ctx.resolve_type(&param.type_.type_)?;
            let stack_index = ctx.stack_reserve_slot();
            ctx.scopes.last_mut().unwrap().names.push(ScopedName {
                name: param.name.token.text,
                type_,
                storage: ValueStorage::Local { stack_index },
            });
        }

        result.extend(self.body.compile_to_ir(ctx)?);

        result.push(ir::Instruction::Label { id: end_label });

        let _scope = ctx.scopes.pop().expect("Scope mismatch");

        // TODO: destructors?

        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeRef {
    pub name: TypeName,
    pub arguments: Option<TypeArgumentList>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeArgumentList {
    pub open: ParenOpen,
    pub items: Punctuated<TypeRef, Comma>,
    pub close: ParenClose,
}
impl TypeArgumentList {
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ParameterList {
    pub open: ParenOpen,
    pub items: Punctuated<Parameter, Comma>,
    pub close: ParenClose,
}
impl ParameterList {
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Parameter {
    pub name: ValueName,
    pub type_: TypeAnnotation,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub enum Stmt {
    FunctionDefinition(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
    Assignment(AssignmentStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    If(IfStmt),
    While(WhileStmt),
    Expr(ExprStmt),
}
impl CompileToIr for Stmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        match self {
            Self::FunctionDefinition(inner) => inner.compile_to_ir(ctx),
            Self::VariableDeclaration(inner) => inner.compile_to_ir(ctx),
            Self::Assignment(inner) => inner.compile_to_ir(ctx),
            Self::Return(inner) => inner.compile_to_ir(ctx),
            Self::Break(inner) => inner.compile_to_ir(ctx),
            Self::Continue(inner) => inner.compile_to_ir(ctx),
            Self::If(inner) => inner.compile_to_ir(ctx),
            Self::While(inner) => inner.compile_to_ir(ctx),
            Self::Expr(inner) => inner.compile_to_ir(ctx),
        }
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ExprStmt {
    pub expr: Expr,
    pub semicolon: Semicolon,
}
impl CompileToIr for ExprStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = self.expr.compile_to_ir(ctx)?;
        result.push(ir::Instruction::Remove { depth: 0 });
        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ReturnStmt {
    pub keyword: KeywordReturn,
    pub value: Option<Expr>,
    pub semicolon: Semicolon,
}
impl CompileToIr for ReturnStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = Vec::new();

        result.push(ir::Instruction::Comment(format!("Return value")));

        if let Some(expr) = &self.value {
            result.extend(expr.compile_to_ir(ctx)?);
        } else {
            result.push(ir::Instruction::Push { value: Value::Unit });
        }

        // Jump to end ("return")
        let label = ctx
            .scopes
            .iter()
            .rev()
            .find_map(|scope| {
                if let ScopeType::Function { end_label, .. } = scope.type_ {
                    Some(end_label)
                } else {
                    None
                }
            })
            .expect("TODO: proper error: Return outside function");

        result.push(ir::Instruction::Comment(format!("Return")));
        result.push(ir::Instruction::Push {
            value: Value::Boolean(true),
        });
        result.push(ir::Instruction::JumpToIf {
            condition: true,
            label,
        });

        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct BreakStmt {
    pub keyword: KeywordBreak,
    pub value: Option<Expr>,
    pub semicolon: Semicolon,
}
impl CompileToIr for BreakStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        todo!();
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ContinueStmt {
    pub keyword: KeywordContinue,
    pub semicolon: Semicolon,
}
impl CompileToIr for ContinueStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        todo!();
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct WhileStmt {
    pub keyword: KeywordWhile,
    pub condition: Expr,
    pub body: Block,
    pub semicolon: Option<Semicolon>,
}
impl CompileToIr for WhileStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        todo!();
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct IfStmt {
    pub keyword: KeywordIf,
    pub condition: Expr,
    pub body: Block,
    pub semicolon: Option<Semicolon>,
}
impl CompileToIr for IfStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = Vec::new();

        result.push(ir::Instruction::Comment(format!("If condition")));

        result.extend(self.condition.compile_to_ir(ctx)?);

        let end_label = LabelId::new();

        result.push(ir::Instruction::Comment(format!("If condition jump?")));

        result.push(ir::Instruction::JumpToIf {
            condition: false,
            label: end_label,
        });

        result.push(ir::Instruction::Comment(format!("If body")));

        result.extend(self.body.compile_to_ir(ctx)?);

        result.push(ir::Instruction::Comment(format!("If end")));

        result.push(ir::Instruction::Label { id: end_label });

        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct AssignmentStmt {
    pub target: ValueName,
    pub condition: AssignOp,
    pub value: Expr,
    pub semicolon: Semicolon,
}
impl CompileToIr for AssignmentStmt {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        todo!();
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Block {
    pub open: CurlyOpen,
    pub body: Vec<Stmt>,
    pub returns: Option<Expr>,
    pub close: CurlyClose,
}
impl CompileToIr for Block {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = Vec::new();

        result.push(ir::Instruction::Comment(format!("Start of block")));

        for stmt in &self.body {
            result.extend(stmt.compile_to_ir(ctx)?);
        }

        if let Some(r) = &self.returns {
            result.extend(r.compile_to_ir(ctx)?);
        } else {
            result.push(ir::Instruction::Push { value: Value::Unit });
        }

        result.push(ir::Instruction::Comment(format!("End of block")));

        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct VariableDeclaration {
    pub let_: KeywordLet,
    pub name: ValueName,
    pub type_: Option<TypeAnnotation>,
    pub assign_op: AssignOp,
    /// TODO: without default value
    pub value: Expr,
    pub semicolon: Semicolon,
}
impl CompileToIr for VariableDeclaration {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let stack_index = ctx.stack_reserve_slot();
        let scope = ctx.scopes.last_mut().unwrap();
        scope.names.push(ScopedName {
            name: self.name.token.text.clone(),
            type_: self
                .type_
                .as_ref()
                .map(|t| DataType::from_type_ref(&t.type_))
                .unwrap_or(Ok(DataType::Infer))?,
            storage: ValueStorage::Local { stack_index },
        });
        let depth = ctx.stack_index_to_depth(stack_index);
        let mut result = self.value.compile_to_ir(ctx)?;
        result.push(ir::Instruction::Comment(format!("Move to correct place")));
        result.push(ir::Instruction::Swap { depth });
        result.push(ir::Instruction::Remove { depth: 0 });
        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeAnnotation {
    pub colon: Colon,
    pub type_: TypeRef,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct FunctionCall {
    pub function: ValueName,
    pub arguments: ArgumentList,
}
impl CompileToIr for FunctionCall {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        let mut result = Vec::new();
        for arg in self.arguments.items.iter() {
            result.extend(arg.compile_to_ir(ctx)?);
        }
        // TODO: typecheck
        result.extend(
            ctx.resolve_name(&self.function)?
                .storage
                .compile_to_ir(ctx)?,
        );
        result.push(ir::Instruction::CallFunction {
            argc: self.arguments.items.len(),
        });

        Ok(result)
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ArgumentList {
    pub open: ParenOpen,
    pub items: Punctuated<Expr, Comma>,
    pub close: ParenClose,
}
impl ArgumentList {
    pub fn len(&self) -> usize {
        self.items.len()
    }
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub enum Expr {
    FunctionCall(FunctionCall),
    // Branch(Branch),
    Literal(LiteralInteger),
    Name(ValueName),
}
impl CompileToIr for Expr {
    fn compile_to_ir(
        &self,
        ctx: &mut CompileContext,
    ) -> Result<Vec<ir::Instruction>, CompileError> {
        Ok(match self {
            Self::FunctionCall(fnc) => fnc.compile_to_ir(ctx)?,
            Self::Literal(lit) => vec![ir::Instruction::Push {
                value: Value::from_literal(lit)?,
            }],
            Self::Name(name) => ctx.resolve_name(name)?.storage.compile_to_ir(ctx)?,
        })
    }
}
