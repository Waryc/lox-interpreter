use std::env;
use std::fs;
use std::process;

use lalrpop_util::lalrpop_mod;
use lexer::Token;
use logos::Logos;

mod ast;
mod error;
mod lexer;
mod interpreter;

lalrpop_mod!(grammar);

fn main() {
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <source_file>", args[0]);
        process::exit(1);
    }

    // 读取源文件
    let source = match fs::read_to_string(&args[1]) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file {}: {}", args[1], err);
            process::exit(1);
        }
    };

    // 词法分析 - 直接使用 logos
    let lexer = lexer::Token::lexer(&source)
        .spanned()
        .map(|(token_result, span)| {
            match token_result {
                Ok(token) => Ok((span.start, token, span.end)),
                Err(()) => Ok((span.start, Token::Error, span.end))
            }
        });
    
    // 语法分析生成 AST
    let mut errors = Vec::new();
    let parser = grammar::ProgramParser::new();
    let ast = match parser.parse(&mut errors, lexer) {
        Ok(ast) => ast,
        Err(err) => {
            // Extract the inner tuple from the ParseError
            let (custom_err, span) = match err {
                lalrpop_util::ParseError::User { error } => error,
                _ => {
                    eprintln!("Unexpected parse error: {:?}", err);
                    process::exit(1);
                }
            };
            error::report_errors(&mut std::io::stderr(), &source, &[(custom_err, span)]);
            process::exit(1);
        }
    };

    // 打印 AST（使用 Debug 格式化）
    println!("AST:\n{:#?}", ast);

    // let mut interpreter = interpreter::Interpreter::new();
    // match interpreter.interpret(ast.stmts) {
    //     Ok(_) => (),
    //     Err(e) => {
    //         eprintln!("Runtime error: {:?}", e);
    //         process::exit(70);
    //     }
    // }
}