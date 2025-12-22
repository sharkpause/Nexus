use crate::token::Token;

pub enum LexerError {
    UnexpectedChar(char),
    EndOfInput
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

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        while let Some(character) = self.peek_char(0) {
            if character.is_whitespace() {
                self.consume_char();
            } else {
                break;
            }
        }

        let current_char =
            self.peek_char(0).ok_or(LexerError::EndOfInput)?;

        let mut token = String::new();
        if current_char.is_digit(10) {
            while let Some(character) = self.peek_char(0) {
                if character.is_digit(10) {
                    token.push(character);
                    self.consume_char();
                } else {
                    break;
                }
            }
        }

        let number: i64 =
            token.parse().map_err(
                |_| LexerError::UnexpectedChar(
                    token.chars().last().unwrap_or(' ')
                ))?;

        return Ok(Token::IntLiteral(number));
    }
}