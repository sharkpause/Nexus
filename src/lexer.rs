use crate::token::Token;

#[derive(Debug)]
pub enum LexerError {
    GenericError,
    UnexpectedChar(char),
    EndOfInput,
    UnknownToken
}

pub struct Lexer {
    pub input: String,
    pub index: usize
}

impl Lexer {
    fn peek_char(&self, offset: usize) -> Option<char> {
        return self.input.chars().nth(self.index + offset);
    }

    fn consume_char(&mut self) -> Option<char> {
        let character = self.peek_char(0);
        self.index += 1;
        return character;
    }

    fn skip_whitespace(&mut self) {
        while let Some(character) = self.peek_char(0) {
            if character.is_whitespace() {
                self.consume_char();
            } else {
                break;
            }
        }
    }

    fn single_char_token(&mut self) -> Option<Token> {
        let character = self.peek_char(0)?;

        let token = match character {
            '(' => Token::LeftParentheses,
            ')' => Token::RightParentheses,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ';' => Token::Semicolon,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '=' => Token::Equal,
            _ => return None,
        };

        self.consume_char();
        
        return Some(token);
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let current_char =
            self.peek_char(0).ok_or(LexerError::EndOfInput)?;

        let mut token = String::new();

        if current_char.is_digit(10) {
            while let Some(character) = self.peek_char(0) {
                if character.is_digit(10) {
                    token.push(character);
                    self.consume_char();
                } else {
                    if character.is_alphabetic() {
                        return Err(LexerError::UnexpectedChar(character));
                    }
                    break;
                }
            }

            let number: i64 =
                token.parse().map_err(
                |_| LexerError::UnexpectedChar(
                    token.chars().last().unwrap_or(' ')
                ))?;
        
            return Ok(Token::IntLiteral(number));
        } else if current_char.is_alphanumeric() {
            while let Some(character) = self.peek_char(0) {
                if character.is_alphanumeric() {
                    token.push(character);
                    self.consume_char();
                } else {
                    break;
                }
            }

            match token.as_str() {
                "return" => Ok(Token::Return),
                "function" => Ok(Token::Function),
                "int" => Ok(Token::IntType),
                "var" => Ok(Token::Var),
                _ => Ok(Token::Identifier(token))
            }
        } else {
            if let Some(token) = self.single_char_token() {
                return Ok(token);
            }
            return Err(LexerError::UnknownToken);
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        loop {
            match self.next_token() {
                Ok(token) => {
                    println!("{:?}", token);
                    tokens.push(token);
                },
                Err(LexerError::EndOfInput) => {
                    break;
                },
                Err(error) => {
                    return Err(error);
                }
            }
        }

        return Ok(tokens);
    }
}