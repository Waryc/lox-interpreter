use crate::value::*;
use rand::random;

pub struct NativeFunctions;

impl NativeFunctions {
    // 获取当前时间的秒数
    fn clock() -> LoxCallable {
        LoxCallable::Native {
            arity: 0,
            name: "clock",
            function: |_: Vec<LoxValue>| {
                let now = std::time::SystemTime::now();
                let duration = now.duration_since(std::time::UNIX_EPOCH).unwrap();
                LoxValue::Number(duration.as_secs_f64())
            },
        }
    }

    // 获取一个小于参数n的随机整数
    #[allow(non_snake_case)]
    fn randN() -> LoxCallable {
        LoxCallable::Native {
            arity: 1,
            name: "randN",
            function: |args: Vec<LoxValue>| {
                if let Some(LoxValue::Number(n)) = args.get(0) {
                    if *n < 1.0 {
                        return LoxValue::Nil; // 如果参数小于1，返回nil
                    }
                    let n = n.floor() as u32; // 确保是整数
                    LoxValue::Number((random::<u32>() % n).into())
                } else {
                    LoxValue::Nil // 如果参数不正确，返回nil
                }
            },
        }
    }

    // 睡眠函数，接受一个毫秒数参数
    fn sleep() -> LoxCallable {
        LoxCallable::Native {
            arity: 1,
            name: "sleep",
            function: |args: Vec<LoxValue>| {
                if let Some(LoxValue::Number(milliseconds)) = args.get(0) {
                    if *milliseconds < 0.0 {
                        return LoxValue::Nil; // 如果参数小于0，返回nil
                    }
                    std::thread::sleep(std::time::Duration::from_millis(*milliseconds as u64));
                    LoxValue::Nil // 返回nil表示完成
                } else {
                    LoxValue::Nil // 如果参数不正确，返回nil
                }
            },
        }
    }

    // 获取所有原生函数的方法
    pub fn get_all() -> Vec<LoxCallable> {
        vec![
            Self::clock(),
            Self::randN(),
            Self::sleep(),
        ]
    }
}

