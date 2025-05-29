use lalrpop_util::ParseError;
use std::fmt::{self};
use crate::lexer;

pub type PE = ParseError<(), lexer::Token, &'static str>;

#[macro_export]
macro_rules! report_error {
    ($msg:expr) => {{
        println!("{}", $msg); // 先输出到 stdout
        std::process::exit(0); // 退出代码 0
        // panic!();
    }};
}

#[derive(Debug)]
pub enum ParseErrorMsg {
    // 词法语法分析
    StmtMissingSemicolon(PE),
    FunctionMissingRightParen(PE),
    DeclVarMissingVariableName(PE),
    #[allow(dead_code)]
    UnexpectedCharacter(PE),  // 未正确实现
    BlockMissingRightBrace(PE),
    // 语义分析
    ReturnFromTopLevelCode(), // 顶层代码不能有 return 语句
    #[allow(dead_code)] // 不需要实现，lox 没有 continue/break 语句
    MissplacedContinueBreak(String), // 语句位置错误
    AlreadyDefinedVariable(),
    SuperclassNotFound(), 
    IllegalSuper(), 
    IllegalThis(),
}

impl fmt::Display for ParseErrorMsg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseErrorMsg::StmtMissingSemicolon(err) => 
                write!(f, "Error at '{}': Expect ';' after value.", pe_reslover(err)),
            ParseErrorMsg::FunctionMissingRightParen(err) => 
                write!(f, "Error at '{}': Expect ')' after parameters.", pe_reslover(err)),
            ParseErrorMsg::DeclVarMissingVariableName(err) => 
                write!(f, "Error at '{}': Expect variable name.", pe_reslover(err)),
            ParseErrorMsg::UnexpectedCharacter(err) =>
                write!(f, "Error at '{}': Unexpected character.", pe_reslover(err)),
            ParseErrorMsg::BlockMissingRightBrace(err) =>
                write!(f, "Error at '{}': Expect '}}' after block.", pe_reslover(err)),
            ParseErrorMsg::ReturnFromTopLevelCode() =>
                write!(f, "Error: Can't return from top-level code."),
            ParseErrorMsg::MissplacedContinueBreak(stmt) =>
                write!(f, "Error: Can't use '{}' outside of a loop.", stmt),
            ParseErrorMsg::AlreadyDefinedVariable() =>
                write!(f, "Error: Already a variable with this name in this scope."),
            ParseErrorMsg::SuperclassNotFound() =>
                write!(f, "Error: Superclass must be a class."),
            ParseErrorMsg::IllegalSuper() =>
                write!(f, "Error: Can't use 'super' in a class with no superclass."),
            ParseErrorMsg::IllegalThis() =>
                write!(f, "Error: Can't use 'this' outside of a class."),
        }
    }
}

fn pe_reslover(error: &PE) -> String {
    match error {
        ParseError::UnrecognizedEof { location: _, expected: _ } => "EOF".to_string(),
        ParseError::UnrecognizedToken { expected: _, token: (_, token, _) } => token.to_string(),
        _ => "".to_string(),
    }
}

pub enum RuntimeError {
    DivisionByZero,
    UndefinedVariable(String),
    UndefinedProperty(String), // 方法名，字段名
    IllegalCall,
    ArityMismatch(usize, usize), // 期望参数个数，实际参数个数
    TypeMismatch,
    StackOverflow,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RuntimeError::DivisionByZero =>
                write!(f, "RuntimeError: Division by zero."),
            RuntimeError::UndefinedVariable(name) =>
                write!(f, "RuntimeError: Undefined variable '{}'.", name),
            RuntimeError::UndefinedProperty(name) =>
                write!(f, "RuntimeError: Undefined property '{}'.", name),
            RuntimeError::IllegalCall =>
                write!(f, "RuntimeError: Can only call functions and classes."),
            RuntimeError::ArityMismatch(expected, actual) =>
                write!(f, "RuntimeError: Expected {} arguments but got {}.", expected, actual),
            RuntimeError::TypeMismatch =>
                write!(f, "RuntimeError: Operands must be two numbers or two strings."),
            RuntimeError::StackOverflow =>
                write!(f, "RuntimeError: Stack overflow."),
        }
    }
}