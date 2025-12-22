pub enum Token {
    Return,
    IntLiteral(i64)
}

pub fn print_token(token: &Token) {
    match token {
        Token::Return => {
            println!("Return token");
        },
        Token::IntLiteral(number) => {
            println!("integer {}", number);
        },
    }
}