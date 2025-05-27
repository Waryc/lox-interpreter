use std::fmt::{self, Display, Formatter};

#[derive(Debug, Default)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(StmtBlock),
    Class(StmtClass),
    Expr(StmtExpr),
    For(Box<StmtFor>),
    Fun(StmtFun),
    If(Box<StmtIf>),
    Print(StmtPrint),
    Return(StmtReturn),
    Var(StmtVar),
    While(Box<StmtWhile>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtBlock {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtClass {
    pub name: String,
    pub super_: Option<Expr>,
    pub methods: Vec<StmtFun>,
}

/// An expression statement evaluates an expression and discards the result.
#[derive(Clone, Debug, PartialEq)]
pub struct StmtExpr {
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtFor {
    pub init: Option<Stmt>,
    pub cond: Option<Expr>,
    pub incr: Option<Expr>,
    pub body: Stmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtFun {
    pub name: String,
    pub params: Vec<String>,
    pub body: StmtBlock,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtIf {
    pub cond: Expr,
    pub then: Stmt,
    pub else_: Option<Stmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtPrint {
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtReturn {
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtVar {
    pub var: Var,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StmtWhile {
    pub cond: Expr,
    pub body: Stmt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Assign(Box<ExprAssign>),
    Call(Box<ExprCall>),
    Get(Box<ExprGet>),
    Infix(Box<ExprInfix>),
    Literal(ExprLiteral),
    Prefix(Box<ExprPrefix>),
    Set(Box<ExprSet>),
    Super(ExprSuper),
    Var(ExprVar),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprAssign {
    pub var: Var,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprCall {
    pub callee: Expr,
    pub args: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprGet {
    pub object: Expr,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprLiteral {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprInfix {
    pub lt: Expr,
    pub op: OpInfix,
    pub rt: Expr,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpInfix {
    Add,
    Subtract,
    Multiply,
    Divide,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    LogicAnd,
    LogicOr,
}

impl Display for OpInfix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op = match self {
            OpInfix::Add => "+",
            OpInfix::Subtract => "-",
            OpInfix::Multiply => "*",
            OpInfix::Divide => "/",
            OpInfix::Less => "<",
            OpInfix::LessEqual => "<=",
            OpInfix::Greater => ">",
            OpInfix::GreaterEqual => ">=",
            OpInfix::Equal => "==",
            OpInfix::NotEqual => "!=",
            OpInfix::LogicAnd => "and",
            OpInfix::LogicOr => "or",
        };
        write!(f, "{op}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprPrefix {
    pub op: OpPrefix,
    pub rt: Expr,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum OpPrefix {
    Negate,
    Not,
}

impl Display for OpPrefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let op = match self {
            OpPrefix::Negate => "-",
            OpPrefix::Not => "!",
        };
        write!(f, "{op}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExprSet {
    pub object: Expr,
    pub name: String,
    pub value: Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprSuper {
    pub super_: Var,
    pub name: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ExprVar {
    pub var: Var,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Var {
    pub name: String,
    /// This field is initialized as [`None`] by the parser, and is later
    /// filled by the resolver.
    pub depth: Option<usize>,
}