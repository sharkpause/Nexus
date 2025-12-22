use crate::token::Token;

enum ParserError {
    EndOfInput
}

enum Statement {
    Return(Expression)
}

enum Expression {
    IntLiteral(i64)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize
}

impl Parser {
    fn parse_program(&mut self) -> Statement {

    }

    fn parse_statement(&mut self) -> Statement {

    }

    fn parse_expression(&mut self) -> Expression {

    }

    fn peek_token(&self, offset: usize) -> Option<Token> {
        return tokens.nth(index + offset).ok_or(EndOfInput);
    }

    fn consume_token(&mut self) {
        self.index += 1;
    }
}