use crate::token::{ Token, TokenKind };

#[derive(Debug)]
pub enum LexerError {
    GenericError,
    UnexpectedChar {
        character: char,
        line: usize,
        column: usize
    },
    EndOfInput,
    UnknownToken {
        character: char,
        line: usize,
        column: usize
    }
}

pub struct Lexer {
    pub input: Vec<char>,
    pub index: usize,
    pub line: usize,
    pub column: usize
}

impl Lexer {
    pub fn from(input: String) -> Self {
        return Self {
            input: input.chars().collect(),
            index: 0,
            line: 1,
            column: 1
        };
    }

    fn peek_char(&self, offset: usize) -> Option<char> {
        return self.input.get(self.index + offset).copied();
    }

    fn consume_char(&mut self) -> Option<char> {
        let character = self.peek_char(0)?;
        self.index += 1;

        if character == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        return Some(character);
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
        let line = self.line;
        let column = self.column;

        let kind = match character {
            '(' => TokenKind::LeftParentheses,
            ')' => TokenKind::RightParentheses,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ';' => TokenKind::Semicolon,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '=' => TokenKind::Equal,
            ',' => TokenKind::Comma,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            '!' => TokenKind::Not,
            _ => return None,
        };

        self.consume_char();

        return Some(Token {
            kind,
            line,
            column,
        });
    }

    fn double_char_token(&mut self) -> Option<Token> {
        let line = self.line;
        let column = self.column;

        match self.peek_char(0)? {
            '=' => {
                if self.peek_char(1)? == '=' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::DoubleEqual,
                        line,
                        column,
                    });
                } else {
                    return None;
                }
            },
            '!' => {
                if self.peek_char(1)? == '=' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::NotEqual,
                        line,
                        column,
                    });
                } else {
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::Not,
                        line,
                        column,
                    });
                }
            },
            '<' => {
                if self.peek_char(1)? == '=' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::LessEqual,
                        line,
                        column,
                    });
                } else {
                    return None;
                }
            },
            '>' => {
                if self.peek_char(1)? == '=' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::GreaterEqual,
                        line,
                        column,
                    });
                } else {
                    return None;
                }
            },
            '&' => {
                if self.peek_char(1)? == '&' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::And,
                        line,
                        column,
                    });
                } else {
                    return None;
                }
            },
            '|' => {
                if self.peek_char(1)? == '|' {
                    self.consume_char();
                    self.consume_char();

                    return Some(Token {
                        kind: TokenKind::Or,
                        line,
                        column,
                    });
                } else {
                    return None;
                }
            },
            _ => None,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        loop {
            self.skip_whitespace();

            let current_char =
                self.peek_char(0).ok_or(LexerError::EndOfInput)?;

            let start_line = self.line;
            let start_column = self.column;

            // Comments
            if current_char == '/' {
                if let Some(next_char) = self.peek_char(1) {
                    if next_char == '/' {
                        // single-line comment
                        self.consume_char();
                        self.consume_char();
                        while let Some(c) = self.peek_char(0) {
                            if c == '\n' { break; }
                            self.consume_char();
                        }
                        continue;
                    } else if next_char == '*' {
                        // multi-line comment
                        self.consume_char();
                        self.consume_char();
                        while let Some(c) = self.peek_char(0) {
                            if c == '*' {
                                if let Some(nc) = self.peek_char(1) {
                                    if nc == '/' {
                                        self.consume_char();
                                        self.consume_char();
                                        break;
                                    }
                                }
                            }
                            self.consume_char();
                        }
                        continue;
                    }
                }
            }

            // Number literals
            if current_char.is_digit(10) {
                let mut token = String::new();
                while let Some(character) = self.peek_char(0) {
                    if character.is_digit(10) {
                        token.push(character);
                        self.consume_char();
                    } else if character.is_alphabetic() {
                        return Err(LexerError::UnexpectedChar{
                            character, line: start_line, column: start_column
                        });
                    } else {
                        break;
                    }
                }
                
                // TODO: when the literal is over 128 bits, it returns an unexpectedchar instead of overflow
                let number = token.parse().map_err(|_| LexerError::UnexpectedChar {
                    character: token.chars().last().unwrap_or(' '),
                    line: start_line,
                    column: start_column
                })?;

                return Ok(Token { kind: TokenKind::IntLiteral(number), line: start_line, column: start_column });
            }

            // String literals
            if current_char == '"' {
                self.consume_char();

                let mut string_literal = String::new();
                while let Some(character) = self.peek_char(0) {
                    self.consume_char();
                    if character == '"' {
                        break;
                    }
                    string_literal.push(character);
                }

                return Ok(Token {
                    kind: TokenKind::StringLiteral(string_literal),
                    line: start_line,
                    column: start_column
                })
            }

            // Keywords, identifiers
            if current_char.is_alphabetic() || current_char == '_' {
                let mut token = String::new();
                token.push(current_char);
                self.consume_char();

                while let Some(character) = self.peek_char(0) {
                    if character.is_alphanumeric() || character == '_' {
                        token.push(character);
                        self.consume_char();
                    } else {
                        break;
                    }
                }

                let kind = match token.as_str() {
                    "return" => TokenKind::Return,
                    "function" => TokenKind::Function,
                    "int32" => TokenKind::Int32Type,
                    "int64" => TokenKind::Int64Type,
                    "void" => TokenKind::VoidType,
                    "var" => TokenKind::Var,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    _ => TokenKind::Identifier(token),
                };

                return Ok(Token { kind, line: start_line, column: start_column });
            }

            // --- handle operators / symbols ---
            if let Some(token) = self.double_char_token() {
                return Ok(token);
            }

            if let Some(token) = self.single_char_token() {
                return Ok(token);
            }

            return Err(LexerError::UnknownToken{ character: current_char, line: start_line, column: start_column });
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        loop {
            match self.next_token() {
                Ok(token) => {
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