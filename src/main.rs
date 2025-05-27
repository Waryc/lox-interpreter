use std::env;
use std::fs;
use std::process;
use std::rc::Rc;
use std::cell::RefCell;

use lalrpop_util::lalrpop_mod;
use logos::Logos;

mod ast;
mod environment;
mod error;
mod lexer;
mod interpreter;
mod semantic;
mod value;

use crate::environment::Environment;

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

    // 词法分析
    let lexer = lexer::Token::lexer(&source)
        .map(|result| match result {
            Ok(token) => Ok(token),
            Err(()) => Err("lexical error")  // 将 () 转换为 &str
        });
    
    // 语法分析 
    let parser = grammar::ProgramParser::new();
    let ast = parser.parse(lexer).unwrap();

    // 打印 AST（使用 Debug 格式化）
    println!("AST:\n{:#?}", ast);

    let globals = Rc::new(RefCell::new(Environment::new(None)));

    // 语义分析
    let mut analyzer = semantic::SemanticAnalyzer::new(Rc::clone(&globals));
    analyzer.resolve(&ast);


    // // 解释执行
    // match interpreter.interpret(ast) {
    //     Ok(_) => (),
    //     Err(e) => {
    //         eprintln!("Runtime error: {:?}", e);
    //         process::exit(70);
    //     }
    // }
}