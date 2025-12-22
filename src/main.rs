mod token;
mod lexer;
mod parser;

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let mut lexer = Lexer {
        input: String::from("09103809"),
        index: 0
    };

    let tokens = lexer.tokenize().unwrap();

    println!("Tokens:\n");

    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser {
        tokens: tokens,
        index: 0
    };

    println!("\n\nExpression:");

    let expressions = parser.parse_expression().unwrap();
    
    println!("Expression: {:?}", expressions);

}