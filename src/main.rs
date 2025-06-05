use lalrpop_util::lalrpop_mod;

mod ast;
mod cli;
mod environment;
mod error;
mod interpreter;
mod lexer;
mod native;
mod value;

lalrpop_mod!(grammar);

fn main() {
    // 源码读取
    let source = cli::read_source();

    // 词法分析
    let tokens = lexer::tokenize(&source);

    // 语法分析
    let parser = grammar::ProgramParser::new();
    let ast = parser.parse(tokens).unwrap();

    // 打印 AST
    // println!("AST:\n{:#?}", ast);

    // 解释执行
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.run(&ast);
}