use crate::token::Token;

#[derive(Debug)]
pub enum ParserError {
    EndOfInput,
    GenericError,
    UnexpectedToken(Token),
    UnexpectedEndOfInput,
    UnexpectedType(Type)
}

#[derive(Debug)]
pub enum TopLevel {
    Function(Function),
    Statement(Statement)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    VariableDeclare(Type, String, Expression),
    VariableAssignment(String, Expression),
    Block(Vec<Statement>),
    Expression(Expression),
    Else(Box<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    IntLiteral(i64),
    BinaryOperation(Box<Expression>, Operator, Box<Expression>),
    UnaryOperation(Operator, Box<Expression>),
    FunctionCall(Box<Expression>, Vec<Expression>)
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Add,           // +
    Subtract,      // -
    Multiply,      // *
    Divide,        // /
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // <
    GreaterThan,   // >
    LessEqual,     // <=
    GreaterEqual,  // >=
    And,           // &&
    Or,            // ||
    Not,           // !
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Statement
}

pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize
}

impl Parser {
    pub fn expect_token(&mut self, expected: &Token) -> Result<(), ParserError> {
        match self.peek_token(0) {
            Some(token) if token == expected  => {
                self.consume_token();
                return Ok(());
            },
            Some(token) => {
                return Err(ParserError::UnexpectedToken(token.clone()));
            },
            None => {
                return Err(ParserError::UnexpectedEndOfInput);
            }
        }
    }

    pub fn expect_type(&mut self) -> Result<Type, ParserError> {
        match self.peek_token(0) {
            Some(Token::IntType) => {
                self.consume_token();
                return Ok(Type::Int);
            },
            Some(_type) => {
                return Err(ParserError::UnexpectedToken(_type.clone()));
            },
            None => {
                return Err(ParserError::UnexpectedEndOfInput);
            }
        }
    }

    pub fn expect_identifer(&mut self) -> Result<String, ParserError> {
        match self.peek_token(0) {
            Some(Token::Identifier(value)) => {
                let temp = value.clone();
                self.consume_token();

                return Ok(temp);
            },
            Some(token) => {
                return Err(ParserError::UnexpectedToken(token.clone()));
            },
            None => {
                return Err(ParserError::UnexpectedEndOfInput);
            }
        }
    }

    fn binding_power(&self, token: &Token) -> Option<u8> {
        /*
        highest
        -------
        ! (not)
        * / 
        + -
        < <= > >=
        == !=
        && 
        ||
        -------
        lowest
        */

        return match token {
            Token::Or => Some(1),
            Token::And => Some(2),
            Token::DoubleEqual | Token::NotEqual => Some(3),
            Token::LessThan | Token::LessEqual => Some(4),
            Token::GreaterThan | Token::GreaterEqual => Some(5),
            Token::Plus | Token::Minus => Some(6),
            Token::Star | Token::Slash => Some(7),
            Token::Not => Some(8),
            _ => None
        };
    }

    fn token_to_operator(&self, token: &Token) -> Option<Operator> {
        return match token {
            Token::Plus => Some(Operator::Add),
            Token::Minus => Some(Operator::Subtract),
            Token::Star => Some(Operator::Multiply),
            Token::Slash => Some(Operator::Divide),
            Token::DoubleEqual => Some(Operator::Equal),
            Token::NotEqual => Some(Operator::NotEqual),
            Token::LessThan => Some(Operator::LessThan),
            Token::LessEqual => Some(Operator::LessEqual),
            Token::GreaterThan => Some(Operator::GreaterThan),
            Token::GreaterEqual => Some(Operator::GreaterEqual),
            Token::Not => Some(Operator::Not),
            Token::And => Some(Operator::And),
            Token::Or => Some(Operator::Or),
            _ => None
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

        let return_type = self.expect_type()?;

        let function_name = self.expect_identifer()?;

        self.expect_token(&Token::LeftParentheses)?;

        let mut parameters: Vec<(Type, String)> = Vec::new();
        if !matches!(self.peek_token(0), Some(Token::RightParentheses)) {
            loop {
                let parameter_type = self.expect_type()?;
                let parameter_name = self.expect_identifer()?;
                parameters.push((parameter_type, parameter_name));

                if matches!(self.peek_token(0), Some(Token::Comma)) {
                    self.consume_token();
                } else {
                    break;
                }
            }
        }

        self.consume_token();

        return Ok(Function {
            name: function_name,
            return_type: return_type,
            parameters,
            body: self.parse_block()?
        });
    }

    pub fn parse_block(&mut self) -> Result<Statement, ParserError> {
        self.expect_token(&Token::LeftBrace)?;
        let statements = self.parse_statements()?;

        self.expect_token(&Token::RightBrace)?;

        return Ok(Statement::Block(statements));
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
            Some(Token::LeftBrace) => {
                return self.parse_block();
            },
            Some(Token::Return) => {
                self.consume_token();

                let expression = self.parse_expression(0)?;

                self.expect_token(&Token::Semicolon)?;

                return Ok(Statement::Return(expression));
            },
            Some(Token::Var) => {
                self.consume_token();
                let variable_type = self.expect_type()?;
                let variable_name = self.expect_identifer()?;
                self.expect_token(&Token::Equal)?;
                let initializer = self.parse_expression(0)?;

                self.expect_token(&Token::Semicolon)?;

                return Ok(Statement::VariableDeclare(variable_type, variable_name, initializer));
            },
            Some(Token::Identifier(name)) => {
                let name = name.clone();

                if matches!(self.peek_token(1), Some(Token::Equal)) {
                    self.consume_token(); // name
                    self.consume_token(); // =

                    let expression = self.parse_expression(0)?;
                    self.expect_token(&Token::Semicolon)?;

                    return Ok(Statement::VariableAssignment(name.to_string(), expression));
                } else {
                    let expression = self.parse_expression(0)?;
                    self.expect_token(&Token::Semicolon)?;

                    return Ok(Statement::Expression(expression));
                }
            },
            Some(Token::If) => {
                let mut conditions = Vec::new();
                let mut bodies = Vec::new();

                self.consume_token();

                let condition = self.parse_expression(0)?;
                let body = self.parse_block()?;

                conditions.push(condition);
                bodies.push(body);

                while let Some(Token::Else) = self.peek_token(0) {
                    if let Some(Token::If) = self.peek_token(1) {
                        self.consume_token();
                        self.consume_token();
                        
                        let condition = self.parse_expression(0)?;
                        let body = self.parse_block()?;

                        conditions.push(condition);
                        bodies.push(body);
                    } else {
                        break;
                    }
                }

                let else_body = if let Some(Token::Else) = self.peek_token(0) {
                    self.consume_token();
                    Some(Box::new(self.parse_block()?))
                } else {
                    None
                };

                let mut result = else_body;
                for (condition, body)
                    in conditions.iter().rev()
                            .zip(bodies.iter().rev()) {
                    result = Some(Box::new(Statement::If(condition.clone(), Box::new(body.clone()), result)));
                }

                return Ok(*result.expect("Internal parser error: expected at least one if/else branch. Something fucked up"));
            },
            _ => {
                return Err(ParserError::GenericError);
            }
        }
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> Result<Expression, ParserError> {
        let current_token =
            self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?.clone();

        let mut lhs = match current_token {
            Token::Identifier(name) => {
                self.consume_token();

                // parse function arguments
                if matches!(self.peek_token(0), Some(Token::LeftParentheses)) {
                    self.consume_token();
                    
                    let mut arguments = Vec::new();
                    
                    if !matches!(self.peek_token(0), Some(Token::RightParentheses)) {
                        loop {
                            arguments.push(self.parse_expression(0)?);
                            if matches!(self.peek_token(0), Some(Token::Comma)) {
                                self.consume_token();
                            } else {
                                break;
                            }
                        }
                    }

                    self.expect_token(&Token::RightParentheses)?;
                    
                    Expression::FunctionCall(
                        Box::new(Expression::Variable(name)),
                        arguments
                    )
                } else {
                    Expression::Variable(name)
                }
            },
            Token::IntLiteral(number) => {
                self.consume_token();
                Expression::IntLiteral(number)
            },
            Token::LeftParentheses => {
                self.consume_token();

                let temp = self.parse_expression(0)?;
                self.expect_token(&Token::RightParentheses)?;

                temp
            },
            Token::Minus => {
                self.consume_token();

                let expression = self.parse_expression(6)?;

                Expression::UnaryOperation(Operator::Subtract, Box::new(expression))
            },
            Token::Not => {
                self.consume_token();

                let expression = self.parse_expression(8)?;

                Expression::UnaryOperation(Operator::Not, Box::new(expression))
            }
            _ => {
                return Err(ParserError::UnexpectedToken(current_token));
            }
        };
        loop {
            println!("Current token: {:?}", self.peek_token(0));
            let operator_token =
                self.peek_token(0).cloned().ok_or(ParserError::UnexpectedEndOfInput)?;

            let binding_power = self.binding_power(&operator_token);
            println!("bp: {:?}", binding_power);
            
            match binding_power {
                Some(bp) => {
                    println!("inside binding_power some: {:?}", operator_token);
                    if bp < min_bp {
                        break;
                    }
                    println!("Pass through");

                    self.consume_token();
                    let rhs_min_bp = bp + 1;
                    let rhs =
                        self.parse_expression(rhs_min_bp)?;

                    println!("a");
                    let operator = self.token_to_operator(&operator_token)
                        .ok_or(ParserError::UnexpectedEndOfInput)?;
                    println!("{:?}", operator);

                    lhs = Expression::BinaryOperation(
                        Box::new(lhs),
                        operator,
                        Box::new(rhs)
                    );
                },
                None => {
                    println!("inside binding_power none: {:?}", operator_token);
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