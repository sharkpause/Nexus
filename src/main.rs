mod token;
mod lexer;

use crate::token::Token;
use crate::token::print_token;
use crate::lexer::Lexer;

fn main() {
    let mut lexer = Lexer {
        input: String::from("123"),
        index: 0
    };

    let token = lexer.next_token();
    match token {
        Ok(token) => {
            print_token(&token);
        },
        Err(e) => {
            println!("sumthin fucked");
        }
    }
}