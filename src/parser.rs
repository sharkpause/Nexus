use crate::token::Token;

#[derive(Debug)]
pub enum ParserError {
    EndOfInput,
    GenericError
}

enum Statement {
    Return(Expression)
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64)
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize
}

impl Parser {
    // pub fn parse_program(&mut self) -> Result<Statement, ParserError> {

    // }

    // fn parse_statement(&mut self) -> Result<Statement, ParserError> {

    // }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        match self.peek_token(0) {
            Some(Token::IntLiteral(number)) => {
                let value = *number;
                self.consume_token();
                return Ok(Expression::IntLiteral(value));
            }

            _ => {
                return Err(ParserError::GenericError);
            }
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.index + offset);
    }

    fn consume_token(&mut self) {
        self.index += 1;
    }
}