use std::env;
use std::fs;
use std::process;
use std::io::{self, Read};

pub fn read_source() -> String {
    let args: Vec<String> = env::args().collect();
    
    match args.len() {
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
    }
}