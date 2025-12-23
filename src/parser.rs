use crate::token::Token;

#[derive(Debug)]
pub enum ParserError {
    EndOfInput,
    GenericError,
    UnexpectedToken
}

#[derive(Debug)]
pub enum TopLevel {
    Function(Function),
    Statement(Statement)
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression)
}

#[derive(Debug)]
pub enum Expression {
    IntLiteral(i64),
    BinaryOp(Box<Expression>, Operator, Box<Expression>),
    UnaryOp(Operator, Box<Expression>)
}

#[derive(Debug)]
pub enum Type {
    Int,
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
}
/*

operator  | binding power
----------+--------------
* /       |  2
+ -       |  1

*/

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub body: Vec<Statement>
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize
}

impl Parser {
    pub fn expect_token(&mut self, expected: &Token) -> Result<(), ParserError> {
        match self.peek_token(0) {
            Some(expected) => {
                self.consume_token();
                return Ok(());
            },
            _ => {
                return Err(ParserError::UnexpectedToken);
            }
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, ParserError> {
        let mut program: Vec<TopLevel> = Vec::new();

        while self.index < self.tokens.len() {
            match self.peek_token(0) {
                Some(Token::Function) => {
                    let function = self.parse_function()?;
                    program.push(TopLevel::Function(function));
                },
                _ => {
                    let current_statement = self.parse_statement()?;
                    program.push(TopLevel::Statement(current_statement));
                }
            }
        }

        return Ok(program);
    }

    pub fn parse_function(&mut self) -> Result<Function, ParserError> {
        self.expect_token(&Token::Function)?;

        self.expect_token(&Token::IntType)?;

        let function_name =
            if let Some(Token::Identifier(name)) = self.peek_token(0).cloned() {
                self.consume_token();

                name.clone()
            } else {
                return Err(ParserError::UnexpectedToken);
            };

        self.expect_token(&Token::LeftParentheses)?;
        self.expect_token(&Token::RightParentheses)?;
        self.expect_token(&Token::LeftBrace)?;

        let function_statements = self.parse_statements()?;

        self.expect_token(&Token::RightBrace)?;

        return Ok(Function {
            name: function_name,
            return_type: Type::Int,
            body: function_statements
        });
    }

    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();

        while let Some(token) = self.peek_token(0) {
            if matches!(token, Token::RightBrace) {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        return Ok(statements);
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek_token(0) {
            Some(Token::Return) => {
                self.consume_token();

                let expression = self.parse_expression(0)?;

                match self.peek_token(0) {
                    Some(Token::Semicolon) => {
                        self.consume_token()
                    },
                    _ => {
                        return Err(ParserError::GenericError);
                    }
                }

                return Ok(Statement::Return(expression));
            },
            _ => {
                return Err(ParserError::GenericError);
            }
        }
    }

    fn binding_power(&self, token: &Token) -> Option<u8> {
        return match token {
            Token::Plus | Token::Minus => Some(1),
            Token::Star | Token::Slash => Some(2),
            _ => None
        };
    }

    fn token_to_operator(&self, token: &Token) -> Option<Operator> {
        return match token {
            Token::Plus => Some(Operator::Add),
            Token::Minus => Some(Operator::Subtract),
            Token::Star => Some(Operator::Multiply),
            Token::Slash => Some(Operator::Divide),
            _ => None
        }
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> Result<Expression, ParserError> {
        let current_token =
            self.peek_token(0).ok_or(ParserError::GenericError)?.clone();
        
        let mut lhs = match current_token {
            Token::IntLiteral(number) => {
                self.consume_token();
                Expression::IntLiteral(number)
            },
            Token::LeftParentheses => {
                self.consume_token();

                let temp = self.parse_expression(0)?;
                self.expect_token(&Token::RightParentheses);

                temp
            },
            Token::Minus => {
                self.consume_token();

                let temp = self.parse_expression(3)?;

                Expression::UnaryOp(Operator::Subtract, Box::new(temp))
            },
            _ => {
                return Err(ParserError::UnexpectedToken);
            }
        };
        
        loop {
            let operator =
                self.peek_token(0).cloned().ok_or(ParserError::UnexpectedToken)?;
            
            let binding_power = self.binding_power(&operator);
            
            match binding_power {
                Some(bp) => {
                    if bp < min_bp {
                        break;
                    }

                    self.consume_token();
                    let rhs_min_bp = bp + 1;
                    let rhs =
                        self.parse_expression(rhs_min_bp)?;

                    let operator =
                        self.token_to_operator(&operator).ok_or(ParserError::UnexpectedToken)?;

                    lhs = Expression::BinaryOp(
                        Box::new(lhs),
                        operator,
                        Box::new(rhs)
                    );
                },
                None => {
                    break;
                }
            };
        }

        return Ok(lhs);
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.index + offset);
    }

    fn consume_token(&mut self) {
        self.index += 1;
    }
}