use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::*;
use crate::environment::Environment;
use crate::value::{LoxClass, LoxValue, LoxCallable};
use crate::{error::ParseErrorMsg, report_error};

/// 解析器，构建符号表与作用域链
pub struct SemanticAnalyzer {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>, // 当前作用域
}

impl SemanticAnalyzer {
    pub fn new(globals: Rc<RefCell<Environment>>) -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new(Some(globals.clone())))),
            globals,  // 原始的 globals 最后使用
        }
    }

    /// 主解析入口
    pub fn resolve(&mut self, ast: &Program) {
        self.visit_program(ast);
    }

    fn visit_program(&mut self, program: &Program) {
        for stmt in &program.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => self.visit_block(block),
            Stmt::Class(class) => self.visit_class(class),
            Stmt::Expr(expr_stmt) => self.visit_expr(&expr_stmt.value),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::Fun(func) => self.visit_function(func),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::Print(print_stmt) => self.visit_print(print_stmt),
            Stmt::Return(return_stmt) => self.visit_return(return_stmt),
            Stmt::Var(var_stmt) => self.visit_var(var_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
        }
    }

    fn visit_block(&mut self, block: &StmtBlock) {
        let new_env = Rc::new(RefCell::new(self.environment.borrow().enter()));
        let old_env = Rc::clone(&self.environment);
        self.environment = new_env;

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        self.environment = old_env;
    }

    fn visit_class(&mut self, class: &StmtClass) {
        let mut lox_class = LoxClass {
            name: class.name.clone().to_string(),
            methods: HashMap::new(),
            superclass: None,
        };

        self.environment.borrow_mut().define(
            class.name.clone(),
            LoxValue::Class(Rc::new(lox_class.clone()))
        );

        // 为方便处理，创建一个新的环境用于类内部作用域，同时增加 super 符号
        let new_env = Rc::new(RefCell::new(self.environment.borrow().enter()));
        let old_env = Rc::clone(&self.environment);
        self.environment = new_env;

        self.environment.borrow_mut().define(
            "this".to_string(),
            LoxValue::Class(Rc::new(lox_class.clone()))
        );

        if let Some(super_class) = &class.super_ {
            if let Expr::Var(var) = super_class {
                match self.environment.borrow().get(&var.var.name) {
                    Ok(LoxValue::Class(super_class_value)) => {
                        lox_class.superclass = Some(super_class_value.clone());
                        self.environment.borrow_mut().define(
                            "super".to_string(),
                            LoxValue::Class(super_class_value.clone())
                        );
                    },
                    _ => report_error!(ParseErrorMsg::SuperclassNotFound()),
                }
            }
        }

        for method in &class.methods {
            self.visit_function(method);
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

        self.environment = old_env;
    }

    // 语义分析阶段不需要类型检查
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign(assign) => {
                self.visit_expr(&assign.value);
            },
            Expr::Call(call) => {
                self.visit_expr(&call.callee);
                for arg in &call.args {
                    self.visit_expr(arg);
                }
            },
            Expr::Get(get) => {
                self.visit_expr(&get.object);
            },
            Expr::Infix(infix) => {
                self.visit_expr(&infix.lt);
                self.visit_expr(&infix.rt);
            },
            Expr::Literal(_literal) => {
                // 直接处理字面量，不需要进一步分析
            },
            Expr::Prefix(prefix) => {
                self.visit_expr(&prefix.rt);
            },
            Expr::Set(set) => {
                self.visit_expr(&set.object);
                self.visit_expr(&set.value);
            },
            Expr::Super(_super) => {
                if self.environment.borrow().get("super").is_err() {
                    report_error!(ParseErrorMsg::IllegalSuper());
                }
            },
            Expr::Var(var) => {
                if var.var.name == "this" {
                    if self.environment.borrow().get("this").is_err() {
                        report_error!(ParseErrorMsg::IllegalThis());
                    }
                }
            }
        }
    }

    // for 不创建新作用域
    fn visit_for(&mut self, for_stmt: &StmtFor) {
        if let Some(init) = &for_stmt.init {
            self.visit_stmt(init);
        }

        if let Some(cond) = &for_stmt.cond {
            self.visit_expr(cond);
        }

        if let Some(incr) = &for_stmt.incr {
            self.visit_expr(incr);
        }

        self.visit_stmt(&for_stmt.body);
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

        let new_env = Rc::new(RefCell::new(self.environment.borrow().enter()));
        let old_env = Rc::clone(&self.environment);
        self.environment = new_env;

        for param in &func.params {
            self.environment.borrow_mut().define(param.clone(), LoxValue::Nil); // 初始化参数为 Nil
        }
        self.visit_block(&func.body);

        self.environment = old_env;
    }

    fn visit_if(&mut self, if_stmt: &StmtIf) {
        self.visit_expr(&if_stmt.cond);
        self.visit_stmt(&if_stmt.then);
        if let Some(else_stmt) = &if_stmt.else_ {
            self.visit_stmt(else_stmt);
        }
    }

    fn visit_print(&mut self, print_stmt: &StmtPrint) {
        self.visit_expr(&print_stmt.value);
    }

    fn visit_return(&mut self, return_stmt: &StmtReturn) {
        if Rc::ptr_eq(&self.environment, &self.globals) {
            report_error!(ParseErrorMsg::ReturnFromTopLevelCode());
        }

        if let Some(value) = &return_stmt.value {
            self.visit_expr(value);
        }
    }

    fn visit_var(&mut self, var_stmt: &StmtVar) {
        if self.environment.borrow().get(&var_stmt.var.name).is_ok() {
            report_error!(ParseErrorMsg::AlreadyDefinedVariable());
        }

        let value = if let Some(expr) = &var_stmt.value {
            self.visit_expr(expr);
            LoxValue::Nil 
        } else {
            LoxValue::Nil
        };

        self.environment.borrow_mut().define(var_stmt.var.name.clone(), value);
    }

    fn visit_while(&mut self, while_stmt: &StmtWhile) {
        self.visit_expr(&while_stmt.cond);
        self.visit_stmt(&while_stmt.body);
    }
}