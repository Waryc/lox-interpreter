use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::StmtFun;
use crate::environment::Environment;
use std::fmt;

/// 所有 Lox 运行时值的枚举表示
#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<str>),
    Callable(LoxCallable),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Number(n) => write!(f, "{}", n),
            LoxValue::String(s) => write!(f, "{}", s),
            LoxValue::Callable(c) => write!(f, "{:?}", c),
            LoxValue::Class(c) => write!(f, "class {}", c.name),
            LoxValue::Instance(i) => write!(f, "instance of {}", i.borrow().class.name),
        }
    }
}

/// 可调用对象（函数/方法）
#[derive(Debug, Clone, PartialEq)]  
pub enum LoxCallable {
    /// 原生函数（如 clock()）
    #[allow(dead_code)]    
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
#[derive(Debug, Clone, PartialEq)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, LoxCallable>,
    pub superclass: Option<Rc<LoxClass>>, // 单继承
}

impl LoxClass {
    pub fn resolve_method(&self, name: &str) -> Option<&LoxCallable> {
        self.methods.get(name).or_else(|| {
            self.superclass.as_ref().and_then(|superclass| superclass.resolve_method(name))
        })
    }
}

/// 类实例
#[derive(Debug, Clone, PartialEq)]
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