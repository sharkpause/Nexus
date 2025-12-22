pub enum Token {
    Return,
    IntLiteral(i64),
    Semicolon
}

pub fn print_token(token: &Token) {
    match token {
        Token::Return => {
            println!("Return token");
        },
        Token::IntLiteral(number) => {
            println!("integer {}", number);
        },
        Token::Semicolon => {
            println!("Semicolon token");
        }
    }
}