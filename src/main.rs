mod token;
mod lexer;

use crate::token::print_token;
use crate::lexer::Lexer;

fn main() {
    let mut lexer = Lexer {
        input: String::from("return 0;;"),
        index: 0
    };

    let tokens = lexer.tokenize();
    match tokens {
        Ok(tokens) => {
            for token in &tokens {
                print_token(token);
            }
        },
        Err(error) => {
            println!("sumthin fucked\n{:?}", error);
        }
    }
}