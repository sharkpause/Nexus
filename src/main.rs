mod token;
mod lexer;
mod parser;
mod codegen;

use std::{env, fs};

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::codegen;

fn read_file(path: &String) -> String {
    let source_code =
        fs::read_to_string(path).expect("Failed to read source code");

    return source_code;
}

fn write_file(path: String, contents: &String) {
    fs::write(path, contents).expect("Failed to write assembly");
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Incorrect usage");
        return;
    }

    let mut lexer = Lexer {
        input: read_file(&args[1]),
        index: 0
    };

    let tokens = lexer.tokenize().unwrap();

    println!("Tokens:");

    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser {
        tokens: tokens,
        index: 0
    };

    let program = parser.parse_program().unwrap();
    println!("\nStatements:\n{:?}", program);

    let output = codegen(program);
    write_file(String::from("out.asm"), &output);
}