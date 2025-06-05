use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::StmtFun;
use crate::environment::Environment;
use std::fmt;

#[derive(Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Number(f64),
    String(Rc<str>),
    Callable(LoxCallable),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
    FUCKOFF,
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Number(n) => write!(f, "{}", n),
            LoxValue::String(s) => {
                let processed = process_string_escapes(s);
                write!(f, "{}", processed)
            },
            LoxValue::Callable(c) => match c {
                LoxCallable::User { name, .. } => write!(f, "<fn {}>", name),
                LoxCallable::Method { .. } => write!(f, "<fn method>"),
                LoxCallable::Native { .. } => write!(f, "<native fn>"),
            },
            LoxValue::Class(c) => write!(f, "{}", c.name),
            LoxValue::Instance(i) => write!(f, "{} instance", i.borrow().class.name),
            _ => write!(f, "<unknown>"),
        }
    }
}

/// 处理字符串中的转义序列
fn process_string_escapes(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next_ch) = chars.peek() {
                chars.next(); // 消费下一个字符
                match next_ch {
                    'n' => result.push('\n'),      // 换行
                    'f' => result.push_str("\x1B[2J\x1B[H"),    // 清屏
                    't' => result.push('\t'),      // 制表符
                    'r' => result.push('\r'),      // 回车
                    '\\' => result.push('\\'),     // 反斜杠
                    '"' => result.push('"'),       // 双引号
                    '\'' => result.push('\''),     // 单引号
                    '0' => result.push('\0'),      // 空字符
                    _ => {
                        // 未知转义序列，保持原样
                        result.push('\\');
                        result.push(next_ch);
                    }
                }
            } else {
                result.push('\\');
            }
        } else {
            result.push(ch);
        }
    }
    
    result
}

/// 可调用对象（函数/方法）
#[derive(Clone, PartialEq)]  
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
#[derive(Clone, PartialEq)]
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
#[derive(Clone, PartialEq)]
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