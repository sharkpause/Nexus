mod token;
mod lexer;
mod parser;
mod codegen;

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::codegen::codegen;

fn main() {
    let mut lexer = Lexer {
        input: String::from("return 67;"),
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

    let statements = parser.parse_program().unwrap();
    println!("\nStatements:\n{:?}", statements);

    let output = codegen(statements);
    println!("\nAssembly output:\n{}", output);
}