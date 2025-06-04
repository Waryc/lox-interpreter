use lalrpop_util::ParseError;
use std::fmt::{self};
use crate::lexer;

pub type PE = ParseError<(), lexer::Token, &'static str>;

#[macro_export]
macro_rules! report_error {
    ($msg:expr) => {{
        println!("{}", $msg);
        std::process::exit(0);
    }};
}

pub enum LoxError {
    // 词法错误
    UnexpectedCharacter(char),
    
    // 语法错误
    StmtMissingSemicolon(PE),
    FunctionMissingRightParen(PE),
    DeclVarMissingVariableName(PE),
    BlockMissingRightBrace(PE),
    
    // 语义错误（同解释运行时错误）
    ReturnFromTopLevelCode,
    #[allow(dead_code)]
    MissplacedContinueBreak(String), // 非 lox 语法
    AlreadyDefinedVariable,
    SuperclassNotFound,
    IllegalSuper,
    IllegalThis,
    
    // 运行时错误
    DivisionByZero,
    UndefinedVariable(String),
    UndefinedProperty(String),
    IllegalCall,
    ArityMismatch(usize, usize),
    TypeMismatch,
    StackOverflow,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // 词法错误
            LoxError::UnexpectedCharacter(c) => 
                write!(f, "Error: Unexpected character '{}'.", c),
                
            // 语法错误
            LoxError::StmtMissingSemicolon(err) => 
                write!(f, "Error at '{}': Expect ';' after value.", pe_resolver(err)),
            LoxError::FunctionMissingRightParen(err) => 
                write!(f, "Error at '{}': Expect ')' after parameters.", pe_resolver(err)),
            LoxError::DeclVarMissingVariableName(err) => 
                write!(f, "Error at '{}': Expect variable name.", pe_resolver(err)),
            LoxError::BlockMissingRightBrace(err) =>
                write!(f, "Error at '{}': Expect '}}' after block.", pe_resolver(err)),
                
            // 语义错误
            LoxError::ReturnFromTopLevelCode =>
                write!(f, "Error: Can't return from top-level code."),
            LoxError::MissplacedContinueBreak(stmt) =>
                write!(f, "Error: Can't use '{}' outside of a loop.", stmt),
            LoxError::AlreadyDefinedVariable =>
                write!(f, "Error: Already a variable with this name in this scope."),
            LoxError::SuperclassNotFound =>
                write!(f, "Error: Superclass must be a class."),
            LoxError::IllegalSuper =>
                write!(f, "Error: Can't use 'super' in a class with no superclass."),
            LoxError::IllegalThis =>
                write!(f, "Error: Can't use 'this' outside of a class."),
                
            // 运行时错误
            LoxError::DivisionByZero =>
                write!(f, "RuntimeError: Division by zero."),
            LoxError::UndefinedVariable(name) =>
                write!(f, "RuntimeError: Undefined variable '{}'.", name),
            LoxError::UndefinedProperty(name) =>
                write!(f, "RuntimeError: Undefined property '{}'.", name),
            LoxError::IllegalCall =>
                write!(f, "RuntimeError: Can only call functions and classes."),
            LoxError::ArityMismatch(expected, actual) =>
                write!(f, "RuntimeError: Expected {} arguments but got {}.", expected, actual),
            LoxError::TypeMismatch =>
                write!(f, "RuntimeError: Operands must be two numbers or two strings."),
            LoxError::StackOverflow =>
                write!(f, "RuntimeError: Stack overflow."),
        }
    }
}

fn pe_resolver(error: &PE) -> String {
    match error {
        ParseError::UnrecognizedEof { .. } => "EOF".to_string(),
        ParseError::UnrecognizedToken { token: (_, token, _), .. } => token.to_string(),
        _ => "unknown".to_string(),
    }
}