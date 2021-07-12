use parsem::{Node, Parsable};

mod token;

pub use self::token::{Location, Token, TokenType};

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

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct PunctuatedValue<T: Parsable<TokenType>, P: Parsable<TokenType>> {
    value: T,
    punct: P,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Module {
    items: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct FunctionDefinition {
    fn_keyword: KeywordFn,
    name: ValueName,
    parameters: ParameterList,
    arrow: Arrow,
    return_type: TypeRef,
    body: Block,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeRef {
    name: TypeName,
    arguments: Option<TypeArgumentList>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeArgumentList {
    open: ParenOpen,
    parameters: Punctuated<TypeRef, Comma>,
    close: ParenClose,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ParameterList {
    open: ParenOpen,
    parameters: Punctuated<Parameter, Comma>,
    close: ParenClose,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Parameter {
    name: ValueName,
    type_: TypeAnnotation,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub enum Stmt {
    FunctionDefinition(FunctionDefinition),
    VariableDeclaration(VariableDeclaration),
    Assignment(AssignmentStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(KeywordContinue),
    If(IfStmt),
    While(WhileStmt),
    Expr(ExprStmt),
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ExprStmt {
    expr: Expr,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ReturnStmt {
    keyword: KeywordReturn,
    value: Option<Expr>,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct BreakStmt {
    keyword: KeywordBreak,
    value: Option<Expr>,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ContinueStmt {
    keyword: KeywordContinue,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct WhileStmt {
    keyword: KeywordWhile,
    condition: Expr,
    body: Block,
    semicolon: Option<Semicolon>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct IfStmt {
    keyword: KeywordIf,
    condition: Expr,
    body: Block,
    semicolon: Option<Semicolon>,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct AssignmentStmt {
    target: ValueName,
    condition: AssignOp,
    value: Expr,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct Block {
    open: CurlyOpen,
    body: Vec<Stmt>,
    close: CurlyClose,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct VariableDeclaration {
    let_: KeywordLet,
    name: ValueName,
    type_: Option<TypeAnnotation>,
    assign_op: AssignOp,
    /// TODO: without default value
    value: Expr,
    semicolon: Semicolon,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct TypeAnnotation {
    colon: Colon,
    type_: TypeRef,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct FunctionCall {
    function: ValueName,
    arguments: ArgumentList,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub struct ArgumentList {
    open: ParenOpen,
    arguments: Punctuated<Expr, Comma>,
    close: ParenClose,
}

#[derive(Debug, Clone, Node)]
#[token_type(TokenType)]
pub enum Expr {
    FunctionCall(FunctionCall),
    // Branch(Branch),
    Literal(LiteralInteger),
    Name(ValueName),
}
