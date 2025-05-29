use std::env;
use std::fs;
use std::process;
use std::io::{self, Read};

use lalrpop_util::lalrpop_mod;
use logos::Logos;

mod ast;
mod environment;
mod error;
mod interpreter;
mod lexer;
mod native;
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
    let mut lex = lexer::Token::lexer(&source);
    let tokens: Vec<_> = std::iter::from_fn(|| {
        match lex.next() {
            Some(Ok(token)) => Some(token),
            Some(Err(())) => {
                let span = lex.span();
                let ch = source.chars().nth(span.start).unwrap_or('?');
                report_error!(error::LoxError::UnexpectedCharacter(ch));
            },
            None => None,
        }
    }).collect();

    // 语法分析
    let parser = grammar::ProgramParser::new();
    let ast = parser.parse(tokens).unwrap();

    // 打印 AST
    // println!("AST:\n{:#?}", ast);

    // 解释执行
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.resolve(&ast);
}