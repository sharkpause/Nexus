pub enum Token {
    Return,
    IntLiteral(i64),
    Semicolon,
    LeftBrace,
    RightBrace,
    LeftParentheses,
    RightParentheses,
    Function,
    IntType,
    Identifier(String)
}

pub fn print_token(token: &Token) {
    match token {
        Token::Return => {
            println!("Return");
        },
        Token::IntLiteral(number) => {
            println!("integer {}", number);
        },
        Token::Semicolon => {
            println!("Semicolon");
        },
        Token::LeftBrace => {
            println!("Left brace");
        },
        Token::RightBrace => {
            println!("Right brace");
        },
        Token::LeftParentheses => {
            println!("Left parentheses");
        },
        Token::RightParentheses => {
            println!("Right parentheses");
        },
        Token::Function => {
            println!("Function keyword");
        },
        Token::IntType => {
            println!("Integer type");
        }
        Token::Identifier(string) => {
            println!("Identifier {}", string);
        }
    }
}