// use std::collections::HashMap;
// use crate::ast::{self, Expr, ExprLiteral, Stmt, OpInfix, OpPrefix};

// // 定义可能的运行时错误
// #[derive(Debug)]
// pub enum RuntimeError {
//     TypeError(String),
//     UndefinedVariable(String),
//     ReturnValue(Value),
//     // 其他运行时错误...
// }

// pub type Result<T> = std::result::Result<T, RuntimeError>;

// // 运行时值类型
// #[derive(Debug, Clone)]
// pub enum Value {
//     Number(f64),
//     String(String),
//     Boolean(bool),
//     Nil,
//     // 后续添加函数和类...
// }

// // 环境(作用域)
// #[derive(Debug)]
// struct Environment {
//     values: HashMap<String, Value>,
//     enclosing: Option<Box<Environment>>,
// }

// impl Environment {
//     fn new() -> Self {
//         Self {
//             values: HashMap::new(),
//             enclosing: None,
//         }
//     }

//     fn new_with_enclosing(enclosing: Environment) -> Self {
//         Self {
//             values: HashMap::new(),
//             enclosing: Some(Box::new(enclosing)),
//         }
//     }

//     fn define(&mut self, name: String, value: Value) {
//         self.values.insert(name, value);
//     }

//     fn get(&self, name: &str) -> Result<Value> {
//         if let Some(value) = self.values.get(name) {
//             Ok(value.clone())
//         } else if let Some(enclosing) = &self.enclosing {
//             enclosing.get(name)
//         } else {
//             Err(RuntimeError::UndefinedVariable(name.to_string()))
//         }
//     }
// }

// // 解释器
// #[derive(Debug)]
// pub struct Interpreter {
//     environment: Environment,
// }

// impl Interpreter {
//     pub fn new() -> Self {
//         Self {
//             environment: Environment::new(),
//         }
//     }

//     // 入口函数
//     pub fn interpret(&mut self, statements: Vec<ast::StmtS>) -> Result<()> {
//         for stmt in statements {
//             self.execute_statement(stmt)?;
//         }
//         Ok(())
//     }

//     // 执行语句
//     fn execute_statement(&mut self, stmt: ast::StmtS) -> Result<()> {
//         match stmt.0 {
//             ast::Stmt::Block(_) => todo!(),
//             ast::Stmt::Class(_) => todo!(),
//             ast::Stmt::Expr(_) => todo!(),
//             ast::Stmt::Fun(_) => todo!(),
//             ast::Stmt::If(_) => todo!(),
//             ast::Stmt::Print(_) => todo!(),
//             ast::Stmt::Return(_) => todo!(),
//             ast::Stmt::Var(_) => todo!(),
//             ast::Stmt::While(_) => todo!(),
//         }
//     }

//     // 执行表达式
//     fn evaluate_expression(&mut self, expr: ast::ExprS) -> Result<Value> {
//         match expr.0 {
//             ast::Expr::Assign(_) => todo!(),
//             ast::Expr::Call(_) => todo!(),
//             ast::Expr::Get(_) => todo!(),
//             ast::Expr::Infix(_) => todo!(),
//             ast::Expr::Literal(_) => todo!(),
//             ast::Expr::Prefix(_) => todo!(),
//             ast::Expr::Set(_) => todo!(),
//             ast::Expr::Super(_) => todo!(),
//             ast::Expr::Var(_) => todo!(),
//         }
//     }
// }