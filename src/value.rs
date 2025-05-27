use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

use crate::ast::StmtFun;
use crate::environment::Environment;

/// 所有 Lox 运行时值的枚举表示
#[derive(Debug, Clone)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<str>),
    Callable(LoxCallable),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
}

/// 可调用对象（函数/方法）
#[derive(Debug, Clone)]
pub enum LoxCallable {
    /// 原生函数（如 clock()）
    Native {
        arity: usize,
        name: &'static str,
        function: fn(Vec<LoxValue>) -> LoxValue,
    },

    /// 用户定义函数
    User {
        arity: usize,
        name: String,
        declaration: StmtFun, 
        closure: Rc<RefCell<Environment>>, 
    },

    /// 方法绑定
    Method {
        receiver: Rc<RefCell<LoxInstance>>,
        method: Rc<LoxCallable>,
    },
}

/// 类定义
#[derive(Debug, Clone)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, LoxCallable>,
    pub superclass: Option<Rc<LoxClass>>, // 单继承
}

/// 类实例
#[derive(Debug, Clone)]
pub struct LoxInstance {
    pub class: Rc<LoxClass>,
    pub fields: HashMap<String, LoxValue>, // 动态字段存储
}

// --- 真值判断 ---
impl LoxValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Nil | LoxValue::Bool(false) => false,
            _ => true,
        }
    }
}