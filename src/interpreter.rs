use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::*;
use crate::environment::Environment;
use crate::value::*;
use crate::{error::ParseErrorMsg, error::RuntimeError, report_error};

/// 解析器，构建符号表与作用域链
pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>, // 当前作用域
    call_depth: usize, // 调用深度
}

const MAX_CALL_DEPTH: usize = 1024;

macro_rules! increase_call_depth {
    ($self:expr) => {{
        $self.call_depth += 1;
        if $self.call_depth > MAX_CALL_DEPTH {
            report_error!(RuntimeError::StackOverflow);
        }
    }};
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new(None)));
        Interpreter {
            globals: globals.clone(),
            environment: globals.clone(),
            call_depth: 0,
        }
    }

    pub fn resolve(&mut self, ast: &Program) {
        self.visit_program(ast);
    }

    fn visit_program(&mut self, program: &Program) {
        for stmt in &program.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Option<LoxValue> {
        match stmt {
            Stmt::Block(block) => return self.visit_block(block),
            Stmt::Class(class) => self.visit_class(class),
            Stmt::Expr(expr_stmt) => _ = self.visit_expr(&expr_stmt.value),
            Stmt::For(for_stmt) => return self.visit_for(for_stmt),
            Stmt::Fun(func) => self.visit_function(func),
            Stmt::If(if_stmt) => return self.visit_if(if_stmt),
            Stmt::Print(print_stmt) => self.visit_print(print_stmt),
            Stmt::Return(return_stmt) => return self.visit_return(return_stmt),
            Stmt::Var(var_stmt) => self.visit_var(var_stmt),
            Stmt::While(while_stmt) => return self.visit_while(while_stmt),
        }
        None
    }

    fn visit_block(&mut self, block: &StmtBlock) -> Option<LoxValue> {
        increase_call_depth!(self);
        let new_env = Environment::new(Some(self.environment.clone()));
        let old_env = std::mem::replace(&mut self.environment, Rc::new(RefCell::new(new_env)));

        let ret = block.stmts.iter().find_map(|stmt| self.visit_stmt(stmt));

        self.environment = old_env;
        self.call_depth -= 1;
        ret
    }

    fn visit_class(&mut self, class: &StmtClass) {
        let mut lox_class = LoxClass {
            name: class.name.clone().to_string(),
            methods: HashMap::new(),
            superclass: None,
        };

        if let Some(super_class) = &class.super_ {
            if let Expr::Var(var) = super_class {
                let _ = match self.environment.borrow().get(&var.var.name) {
                    Ok(LoxValue::Class(super_class_value)) => {
                        lox_class.superclass = Some(super_class_value.clone());
                    },
                    _ => report_error!(RuntimeError::UndefinedVariable(var.var.name.clone())),
                };
            }
        }

        for method in &class.methods {
            lox_class.methods.insert(
                method.name.clone(),
                LoxCallable::User {
                    arity: method.params.len(),
                    name: method.name.clone(),
                    declaration: method.clone(),
                    closure: self.environment.clone(),
                }
            );
        }

        self.environment.borrow_mut().define(
            class.name.clone(),
            LoxValue::Class(Rc::new(lox_class))
        );
    }

    fn visit_expr(&mut self, expr: &Expr) -> LoxValue {
        match expr {
            Expr::Assign(assign) => {
                let value = self.visit_expr(&assign.value);
                self.environment.borrow_mut().assign(&assign.var.name, value.clone())
                    .unwrap_or_else(|_| {
                        report_error!(RuntimeError::UndefinedVariable(assign.var.name.clone()));
                    });
                value
            },
            Expr::Call(call) => {
                let callee = self.visit_expr(&call.callee);
                let args: Vec<LoxValue> = call.args.iter()
                    .map(|arg| self.visit_expr(arg))
                    .collect();
                match callee {
                    LoxValue::Callable(lox_callable) => {
                        match lox_callable {
                            LoxCallable::Native { arity, name:_, function } => {
                                if args.len() != arity {
                                    report_error!(RuntimeError::ArityMismatch(arity, args.len()));
                                }
                                return function(args);
                            },
                            _ => {
                                let (arity, closure, declaration, receiver) = match lox_callable {
                                    LoxCallable::User { arity, closure, declaration, .. } => (arity, closure.clone(), declaration.clone(), None),
                                    LoxCallable::Method { receiver, method } => {
                                        if let LoxCallable::User { arity, declaration, closure, .. } = method.as_ref() {
                                            (*arity, closure.clone(), declaration.clone(), Some(receiver.clone()))
                                        } else {
                                            unreachable!()
                                        }
                                    }
                                    LoxCallable::Native { .. } => {
                                        unreachable!("Native functions should be handled in previous match arm")
                                    }
                                };

                                if args.len() != arity {
                                    report_error!(RuntimeError::ArityMismatch(arity, args.len()));
                                }

                                increase_call_depth!(self);
                                let new_env = Environment::new(Some(closure));
                                let old_env = std::mem::replace(&mut self.environment, Rc::new(RefCell::new(new_env)));

                                if let Some(receiver_ref) = receiver {
                                    self.environment.borrow_mut().define("this".to_string(), LoxValue::Instance(receiver_ref));
                                }

                                for (i, param) in declaration.params.iter().enumerate() {
                                    self.environment.borrow_mut().define(param.clone(), args[i].clone());
                                }

                                let ret = match self.visit_block(&declaration.body) {
                                    Some(value) => value,
                                    None => LoxValue::Nil,
                                };

                                self.environment = old_env;
                                self.call_depth -= 1;
                                ret
                            },
                        }
                    },
                    LoxValue::Class(class) => {
                        let instance = Rc::new(RefCell::new(LoxInstance {
                            class: class.clone(),
                            fields: HashMap::new(),
                        }));

                        if let Some(init_method) = class.resolve_method("init") {
                            if let LoxCallable::User { arity, closure, declaration, .. } = init_method {
                                if args.len() != *arity {
                                    report_error!(RuntimeError::ArityMismatch(*arity, args.len()));
                                }

                                increase_call_depth!(self);
                                let new_env = Environment::new(Some(closure.clone()));
                                let old_env = std::mem::replace(&mut self.environment, Rc::new(RefCell::new(new_env)));

                                self.environment.borrow_mut().define("this".to_string(), LoxValue::Instance(instance.clone()));

                                for (i, param) in declaration.params.iter().enumerate() {
                                    self.environment.borrow_mut().define(param.clone(), args[i].clone());
                                }

                                self.visit_block(&declaration.body);

                                self.environment = old_env;
                                self.call_depth -= 1;
                            }
                        } else {
                            if args.len() > 0 {
                                report_error!(RuntimeError::ArityMismatch(0, args.len()));
                            }
                        }

                        LoxValue::Instance(instance)
                    },
                    _ => report_error!(RuntimeError::IllegalCall),
                }
            },
            Expr::Get(get) => {
                let object = self.visit_expr(&get.object);
                if let LoxValue::Instance(instance) = object {
                    instance.borrow().fields.get(&get.name)
                        .cloned()
                        .unwrap_or_else(|| {
                            let method = instance.borrow().class.resolve_method(&get.name)
                                .unwrap_or_else(|| {
                                    report_error!(RuntimeError::UndefinedProperty(get.name.clone()));
                                })
                                .clone();
                            LoxValue::Callable(LoxCallable::Method {
                                receiver: instance.clone(),
                                method: Rc::new(method),
                            })
                        })
                } else {
                    unreachable!();
                }
            },
            Expr::Infix(infix) => {
                let left = self.visit_expr(&infix.lt);
                let right = self.visit_expr(&infix.rt);
                match infix.op {
                    OpInfix::LogicAnd => return LoxValue::Bool(left.is_truthy() && right.is_truthy()),
                    OpInfix::LogicOr => return LoxValue::Bool(left.is_truthy() || right.is_truthy()),
                    _ => {}
                }
                match (left, right) {
                    // 数值运算
                    (LoxValue::Number(l), LoxValue::Number(r)) => match infix.op {
                        OpInfix::Add => LoxValue::Number(l + r),
                        OpInfix::Subtract => LoxValue::Number(l - r),
                        OpInfix::Multiply => LoxValue::Number(l * r),
                        OpInfix::Divide => {
                            if r == 0.0 { report_error!(RuntimeError::DivisionByZero) }
                            LoxValue::Number(l / r)
                        },
                        OpInfix::Equal => LoxValue::Bool(l == r),
                        OpInfix::NotEqual => LoxValue::Bool(l != r),
                        OpInfix::Less => LoxValue::Bool(l < r),
                        OpInfix::Greater => LoxValue::Bool(l > r),
                        OpInfix::LessEqual => LoxValue::Bool(l <= r),
                        OpInfix::GreaterEqual => LoxValue::Bool(l >= r),
                        _ => report_error!(RuntimeError::TypeMismatch),
                    },
                    // 字符串操作
                    (LoxValue::String(l), LoxValue::String(r)) => match infix.op {
                        OpInfix::Add => LoxValue::String(format!("{}{}", l, r).into()),
                        OpInfix::Equal => LoxValue::Bool(*l == *r),
                        OpInfix::NotEqual => LoxValue::Bool(*l != *r),
                        _ => report_error!(RuntimeError::TypeMismatch),
                    },
                    // 布尔运算
                    (LoxValue::Bool(l), LoxValue::Bool(r)) => match infix.op {
                        OpInfix::Equal => LoxValue::Bool(l == r),
                        OpInfix::NotEqual => LoxValue::Bool(l != r),
                        _ => report_error!(RuntimeError::TypeMismatch),
                    },
                    // Nil 比较
                    (LoxValue::Nil, LoxValue::Nil) => match infix.op {
                        OpInfix::Equal => LoxValue::Bool(true),
                        OpInfix::NotEqual => LoxValue::Bool(false),
                        _ => report_error!(RuntimeError::TypeMismatch),
                    },
                    // 默认情况（类型不匹配）
                    _ => match infix.op {
                        OpInfix::Equal => LoxValue::Bool(false),
                        OpInfix::NotEqual => LoxValue::Bool(true),
                        _ => report_error!(RuntimeError::TypeMismatch),
                    }
                }
            },
            Expr::Literal(literal) => {
                match literal {
                    ExprLiteral::Bool(b) => LoxValue::Bool(*b),
                    ExprLiteral::Nil => LoxValue::Nil,
                    ExprLiteral::Number(n) => LoxValue::Number(*n),
                    ExprLiteral::String(s) => LoxValue::String(Rc::from(s.as_ref())),
                }
            },
            Expr::Prefix(prefix) => {
                let right = self.visit_expr(&prefix.rt);
                match prefix.op {
                    OpPrefix::Negate => {
                        if let LoxValue::Number(n) = right {
                            LoxValue::Number(-n)
                        } else {
                            report_error!(RuntimeError::TypeMismatch);
                        }
                    },
                    OpPrefix::Not => LoxValue::Bool(!right.is_truthy())
                }
            },
            Expr::Set(set) => {
                let obj = self.visit_expr(&set.object);
                let val = self.visit_expr(&set.value);
                if let LoxValue::Instance(instance) = obj {
                    instance.borrow_mut().fields.insert(set.name.clone(), val.clone());
                    return val;
                } else {
                    unreachable!();
                }
            },
            Expr::Super(super_) => {
                let receiver = self.environment.borrow().get("this")
                    .unwrap_or_else(|_| {
                        report_error!(ParseErrorMsg::IllegalSuper());
                    });
                if let LoxValue::Instance(instance) = receiver {
                    if let Some(super_class) = instance.borrow().class.superclass.clone() {
                        if let Some(method) = super_class.methods.get(&super_.name) {
                            return LoxValue::Callable(LoxCallable::Method {
                                receiver: instance.clone(),
                                method: Rc::new(method.clone()),
                            });
                        } else {
                            report_error!(RuntimeError::UndefinedProperty(super_.name.clone()));
                        }
                    } else {
                        report_error!(ParseErrorMsg::IllegalSuper());
                    }
                } else {
                    unreachable!();
                }
            },
            Expr::Var(var) => {
                if var.var.name == "this" {
                    if self.environment.borrow().get("this").is_err() {
                        report_error!(ParseErrorMsg::IllegalThis());
                    }
                }
                self.environment.borrow().get(&var.var.name)
                    .unwrap_or_else(|_| {
                        report_error!(RuntimeError::UndefinedVariable(var.var.name.clone()));
                    })
            }
        }
    }

    fn visit_for(&mut self, for_stmt: &StmtFor) -> Option<LoxValue> {
        if let Some(init) = &for_stmt.init {
            self.visit_stmt(init);
        }
    
        while for_stmt.cond.is_none() || self.visit_expr(&for_stmt.cond.as_ref().unwrap()).is_truthy() {
            if let Some(body_ret) = self.visit_stmt(&for_stmt.body) {
                return Some(body_ret);
            }
            if let Some(incr) = &for_stmt.incr {
                self.visit_expr(incr);
            }
        }
        None
    }

    fn visit_function(&mut self, func: &StmtFun) {
        let lox_callable = LoxCallable::User {
            arity: func.params.len(),
            name: func.name.clone(),
            declaration: func.clone(),
            closure: self.environment.clone(),
        };

        self.environment.borrow_mut().define(
            func.name.clone(),
            LoxValue::Callable(lox_callable)
        );
    }

    fn visit_if(&mut self, if_stmt: &StmtIf) -> Option<LoxValue> {
        if self.visit_expr(&if_stmt.cond).is_truthy() {
            self.visit_stmt(&if_stmt.then)
        } else if let Some(else_stmt) = &if_stmt.else_ {
            self.visit_stmt(else_stmt)
        } else {
            None
        }
    }

    fn visit_print(&mut self, print_stmt: &StmtPrint) {
        print!("{}", self.visit_expr(&print_stmt.value));
        println!();
    }

    fn visit_return(&mut self, return_stmt: &StmtReturn) -> Option<LoxValue> {
        if Rc::ptr_eq(&self.environment, &self.globals) {
            report_error!(ParseErrorMsg::ReturnFromTopLevelCode());
        }

        if let Some(value) = &return_stmt.value {
            Some(self.visit_expr(value))
        } else {
            None
        }
    }

    fn visit_var(&mut self, var_stmt: &StmtVar) {
        if self.environment.borrow().check(&var_stmt.var.name) {
            report_error!(ParseErrorMsg::AlreadyDefinedVariable());
        }

        let value = if let Some(expr) = &var_stmt.value {
            self.visit_expr(expr)
        } else {
            LoxValue::Nil
        };

        self.environment.borrow_mut().define(var_stmt.var.name.clone(), value);
    }

    fn visit_while(&mut self, while_stmt: &StmtWhile) -> Option<LoxValue> {
        while self.visit_expr(&while_stmt.cond).is_truthy() {
            if let Some(body_ret) = self.visit_stmt(&while_stmt.body) {
                return Some(body_ret);
            }
        }
        None
    }
}