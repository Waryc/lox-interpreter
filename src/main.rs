use std::env;
use std::fs;
use std::process;
use std::io::{self, Read};

use lalrpop_util::lalrpop_mod;
use logos::Logos;

mod ast;
mod environment;
mod error;
mod lexer;
mod interpreter;
mod value;

lalrpop_mod!(grammar);

fn main() {
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    
    // 读取源代码
    let source = match args.len() {
        1 => {
            // 没有参数，从标准输入读取
            let mut buffer = String::new();
            match io::stdin().read_to_string(&mut buffer) {
                Ok(_) => buffer,
                Err(err) => {
                    eprintln!("Error reading from stdin: {}", err);
                    process::exit(1);
                }
            }
        },
        2 => {
            // 有一个参数，从文件读取
            match fs::read_to_string(&args[1]) {
                Ok(content) => content,
                Err(err) => {
                    eprintln!("Error reading file {}: {}", args[1], err);
                    process::exit(1);
                }
            }
        },
        _ => {
            eprintln!("Usage: {} [source_file]", args[0]);
            eprintln!("  If no source_file is provided, reads from stdin");
            process::exit(1);
        }
    };

    // 词法分析
    let lexer = lexer::Token::lexer(&source)
        .map(|result| match result {
            Ok(token) => Ok(token),
            Err(()) => Err("lexical error")  // 将 () 转换为 &str
        });
    
    // 语法分析 
    let parser = grammar::ProgramParser::new();
    let ast = parser.parse(lexer).unwrap();

    // 打印 AST
    // println!("AST:\n{:#?}", ast);

    // 解释执行
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.resolve(&ast);
}