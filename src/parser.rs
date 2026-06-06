use crate::{errors::ParserError, lexing::token::{Token, TokenKind}, parsing::{expression::{Expression, ExpressionKind}, function::Function, operator::Operator, span::Span, statement::Statement, toplevel::TopLevel, types::Type}};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub index: usize,
}

impl Parser {
    pub fn from(tokens: Vec<Token>) -> Self {
        // Potentially change to take ownership of tokens instead of copy for performance and memory usage
        return Self {
            tokens,
            index: 0
        };
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        return self.tokens.get(self.index + offset);
    }

    fn consume_token(&mut self) {
        self.index += 1;
    }

    pub fn expect_token(&mut self, expected: &TokenKind) -> Result<Token, ParserError> {
        let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?.clone();
        
         if token.same_kind(expected) {
            self.consume_token();
            return Ok(token);
        } else {
            return Err(ParserError::UnexpectedToken(token.clone()));
        }
    }

    pub fn expect_type(&mut self) -> Result<Type, ParserError> {
        let token = self
            .peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;

        let mut pointer_depth = token.pointer_depth;

        let mut type_ =
            match &token.kind {
                TokenKind::Int8Type => {
                    Type::Int8
                },

                TokenKind::Int32Type => {
                    Type::Int32
                },
                
                TokenKind::Int64Type => {
                    Type::Int64
                },
                
                TokenKind::VoidType => {
                    Type::Void
                },

                TokenKind::StringType => {
                    pointer_depth -= 1; // Because we already wrap it in one pointer
                    Type::Pointer(Box::new(Type::Int8))
                },

                TokenKind::BoolType => {
                    Type::Int1
                }
                
                _ => return Err(ParserError::UnexpectedToken(token.clone())),
            };

        self.consume_token();
        
        for _ in 0..pointer_depth {
            type_ = Type::Pointer(Box::new(type_));
        }

        println!("{:?}", type_);

        return Ok(type_);
        
    }

    pub fn expect_identifier(&mut self) -> Result<String, ParserError> {
        let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?;

        if let TokenKind::Identifier(value) = &token.kind {
            let name = value.clone();
            self.consume_token();
            Ok(name)
        } else {
            Err(ParserError::UnexpectedToken(token.clone()))
        }
    }

    fn binding_power(&self, token: &Token) -> Option<u8> {
        /*
        highest
        -------
        
        ! (not)
        * / %
        + -
        << >>
        < <= > >=
        == !=
        &
        ^
        |
        &&
        ||
        
        -------
        lowest
        */

        return match &token.kind {
            TokenKind::Not => Some(100),

            TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percentage => Some(90),

            TokenKind::Plus | TokenKind::Minus => Some(80),

            TokenKind::ShiftLeft | TokenKind::ShiftRight => Some(70),

            TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual => Some(60),

            TokenKind::DoubleEqual | TokenKind::NotEqual => Some(50),

            TokenKind::Ampersand => Some(40),
            TokenKind::Caret => Some(30),
            TokenKind::Pipe => Some(20),

            TokenKind::And => Some(10),
            TokenKind::Or => Some(0),

            _ => None
        };
    }

    fn token_to_operator(&self, token: &Token) -> Option<Operator> {
        return match &token.kind {
            TokenKind::Plus => Some(Operator::Add),
            TokenKind::Minus => Some(Operator::Subtract),
            TokenKind::Star => Some(Operator::Multiply),
            TokenKind::Slash => Some(Operator::Divide),
            TokenKind::DoubleEqual => Some(Operator::Equal),
            TokenKind::NotEqual => Some(Operator::NotEqual),
            TokenKind::LessThan => Some(Operator::LessThan),
            TokenKind::LessEqual => Some(Operator::LessEqual),
            TokenKind::GreaterThan => Some(Operator::GreaterThan),
            TokenKind::GreaterEqual => Some(Operator::GreaterEqual),
            TokenKind::Not => Some(Operator::Not),
            TokenKind::And => Some(Operator::And),
            TokenKind::Or => Some(Operator::Or),
            TokenKind::Percentage => Some(Operator::Modulo),
            TokenKind::Ampersand => Some(Operator::BitAnd),
            TokenKind::Pipe => Some(Operator::BitOr),
            TokenKind::Caret => Some(Operator::BitXor),
            TokenKind::ShiftLeft => Some(Operator::ShiftLeft),
            TokenKind::ShiftRight => Some(Operator::ShiftRight),
            _ => None
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, ParserError> {
        let mut program: Vec<TopLevel> = Vec::new();

        while self.index < self.tokens.len() {
            let token = self.peek_token(0).ok_or(ParserError::UnexpectedEndOfInput)?;
            match &token.kind {
                TokenKind::Function => {
                    let function = self.parse_function()?;
                    program.push(TopLevel::Function(function));
                },
                
                TokenKind::Extern => {
                    self.consume_token();
                    let function = self.parse_function()?;

                    if function.body.is_some() {
                        return Err(ParserError::UnexpectedBody);
                    }

                    self.expect_token(&TokenKind::Semicolon);

                    program.push(TopLevel::Function(function));
                },

                _ => {
                    let statement = self.parse_statement()?;
                    program.push(TopLevel::Statement(statement));
                }
            }
        }

        Ok(program)
    }

    pub fn parse_function(&mut self) -> Result<Function, ParserError> {
        let start_token = self.expect_token(&TokenKind::Function)?;
        let span = Span {
            line: start_token.line,
            column: start_token.column
        };

        let return_type = self.expect_type()?;

        let function_name = self.expect_identifier()?;

        self.expect_token(&TokenKind::LeftParentheses)?;

        let mut parameters: Vec<(Type, String)> = Vec::new();
        while let Some(token) = self.peek_token(0) {
            if token.same_kind(&TokenKind::RightParentheses) {
                break;
            }
        
            let parameter_type = self.expect_type()?;
            let parameter_name = self.expect_identifier()?;
            parameters.push((parameter_type, parameter_name));
        
            if let Some(token) = self.peek_token(0) {
                if token.same_kind(&TokenKind::Comma) {
                    self.consume_token();
                }
            }
        }

        self.consume_token();

        if self.peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)
            ?
            .same_kind(&TokenKind::Semicolon) {
            return Ok(Function {
                name: function_name,
                return_type: return_type,
                parameters,
                body: None,
                span
            });
        }

        let function_body = self.parse_block()?;
        return Ok(Function {
            name: function_name,
            return_type: return_type,
            parameters,
            body: Some(function_body),
            span
        });
    }

    pub fn parse_block(&mut self) -> Result<Statement, ParserError> {
        let start_token = self.expect_token(&TokenKind::LeftBrace)?;
        let statements = self.parse_statements()?;

        self.expect_token(&TokenKind::RightBrace)?;

        let span = Span {
            line: start_token.line,
            column: start_token.column,
        };

        return Ok(Statement::Block { statements, span });
    }

    pub fn parse_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements = Vec::new();

        while let Some(token) = self.peek_token(0) {
            if token.same_kind(&TokenKind::RightBrace) {
                break;
            }
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        return Ok(statements);
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;
        let span = Span {
            line: token.line,
            column: token.column
        };

        match &token.kind {
            TokenKind::LeftBrace => self.parse_block(),

            TokenKind::Return => {
                self.consume_token();

                match self.expect_token(&TokenKind::Semicolon) {
                    Ok(_) => {
                        return Ok(Statement::Return { value: None, span });
                    },
                    Err(_) => {
                        let expression = self.parse_expression(0)?;
                        self.expect_token(&TokenKind::Semicolon);

                        return Ok(Statement::Return { value: Some(expression), span });   
                    }
                }
            }

            TokenKind::Var => {
                self.consume_token();
                
                let variable_type = self.expect_type()?;
                let variable_name = self.expect_identifier()?;
                
                self.expect_token(&TokenKind::Equal)?;
                let initializer = self.parse_expression(0)?;
                
                self.expect_token(&TokenKind::Semicolon)?;
                
                return Ok(Statement::VariableInitialize {
                    var_type: variable_type,
                    name: variable_name,
                    initializer,
                    span
                });
            }

            TokenKind::If => {
                self.consume_token();
                
                let mut conditions = vec![self.parse_expression(0)?];
                let mut bodies = vec![self.parse_block()?];

                // else if
                while let Some(current_token) = self.peek_token(0) {
                    if current_token.same_kind(&TokenKind::Else) {
                        if let Some(next_token) = self.peek_token(1) {
                            if next_token.same_kind(&TokenKind::If) {
                                self.consume_token(); // else
                                self.consume_token(); // if
                                
                                conditions.push(self.parse_expression(0)?);
                                bodies.push(self.parse_block()?);
                                
                                continue;
                            }
                        }
                        break;
                    } else {
                        break;
                    }
                }

                let else_body = if let Some(tok) = self.peek_token(0) {
                    if tok.same_kind(&TokenKind::Else) {
                        self.consume_token();
                        Some(Box::new(self.parse_block()?))
                    } else {
                        None
                    }
                } else {
                    None
                };

                // parse nested ifs, doing it recursively to avoid stack overflow with deep recursion
                let mut result = else_body;
                for (cond , body) in conditions.iter().rev().zip(bodies.iter().rev()) {
                    result = Some(Box::new(Statement::If {
                        condition: cond.clone(),
                        then_branch: Box::new(body.clone()),
                        else_branch: result,
                        span
                    }));
                }

                return Ok(*result.expect("Expected at least one if/else branch"));
            }

            TokenKind::While => {
                self.consume_token();
                
                self.expect_token(&TokenKind::LeftParentheses)?;
                let condition = self.parse_expression(0)?;
                
                self.expect_token(&TokenKind::RightParentheses)?;
                let body = Box::new(self.parse_statement()?);
                
                return Ok(Statement::While { condition, body, span });
            }

            TokenKind::Break => {
                self.consume_token();
                self.expect_token(&TokenKind::Semicolon)?;
                 
                return Ok(Statement::Break { span });
            }

            TokenKind::Continue => {
                self.consume_token();
                self.expect_token(&TokenKind::Semicolon)?;
                
                return Ok(Statement::Continue{ span });
            }

            TokenKind::Identifier(_) => {
                let name = if let TokenKind::Identifier(n) = &token.kind {
                    n.clone()
                } else { unreachable!() };

                // Evaluate an assignment
                if let Some(next_token) = self.peek_token(1) {
                    if next_token.same_kind(&TokenKind::Equal) {
                        self.consume_token(); // identifier
                        self.consume_token(); // =

                        let expression = self.parse_expression(0)?;
                        
                        self.expect_token(&TokenKind::Semicolon)?;
                        
                        return Ok(Statement::VariableAssignment { name, value: expression, span });
                    }
                }

                // fallback to expression
                let expression = self.parse_expression(0)?;
                self.expect_token(&TokenKind::Semicolon)?;
                
                // An expression evaluated for its side effects. Yeah this looks weird
                // Currently used primarily for function calls where the return value is discarded.
                return Ok(Statement::Expression { expression, span });
            }

            _ => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    pub fn parse_expression(&mut self, min_bp: u8) -> Result<Expression, ParserError> {
        let current_token = self
            .peek_token(0)
            .ok_or(ParserError::UnexpectedEndOfInput)?;

        let span = Span {
            line: current_token.line,
            column: current_token.column
        };

        let lhs_kind = match &current_token.kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.consume_token();
                    
                if !self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::LeftParentheses)) {
                    ExpressionKind::Variable { name, span, type_: None }
                } else {
                    self.consume_token(); // consume '('
                    let mut arguments = Vec::new();
                    
                    while !self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::RightParentheses)) {
                        arguments.push(self.parse_expression(0)?);
                    
                        if self.peek_token(0).map_or(false, |token| token.same_kind(&TokenKind::Comma)) {
                            self.consume_token();
                        } else {
                            break;
                        }
                    }
                
                    self.expect_token(&TokenKind::RightParentheses)?;
                
                    ExpressionKind::FunctionCall {
                        called: Box::new(
                            Expression {
                                kind: ExpressionKind::Variable { name, span, type_: None },
                                type_: None
                            }),
                        arguments,
                        span
                    }
                }
            },

            TokenKind::IntLiteral(number) => {
                let value = *number;
                self.consume_token();
                
                ExpressionKind::IntLiteral { value, span }
            },

            TokenKind::LeftParentheses => {
                self.consume_token();
                let parsed_expression = self.parse_expression(0)?;
                self.expect_token(&TokenKind::RightParentheses)?;
                
                parsed_expression.kind
            },

            TokenKind::Minus => {
                self.consume_token();
                let expression = self.parse_expression(6)?;
                
                ExpressionKind::UnaryOperation {
                    operator: Operator::Subtract,
                    operand: Box::new(expression),
                    span
                }
            },

            TokenKind::Not => {
                self.consume_token();
                let expression = self.parse_expression(8)?;
                
                ExpressionKind::UnaryOperation {
                    operator: Operator::Not,
                    operand: Box::new(expression),
                    span
                }
            },

            TokenKind::StringLiteral(value) => {
                let expression_value = value.clone();

                self.consume_token();
                
                ExpressionKind::StringLiteral {
                    value: expression_value,
                    span
                }
            },

            TokenKind::TrueValue => {
                self.consume_token();

                ExpressionKind::BooleanLiteral { value: true, span }
            },

            TokenKind::FalseValue => {
                self.consume_token();

                ExpressionKind::BooleanLiteral { value: false, span }
            },

            TokenKind::NullValue => {
                self.consume_token();

                ExpressionKind::NullLiteral { span }
            },

            _ => return Err(ParserError::UnexpectedToken(current_token.clone())),
        };

        let mut lhs = Expression { kind: lhs_kind, type_: None };

        loop {
            let operator_token = self
                .peek_token(0)
                .cloned()
                .ok_or(ParserError::UnexpectedEndOfInput)?;

            let bp = self.binding_power(&operator_token);
            println!("\n\n{:?} {:?}\n\n", operator_token, bp);

            match bp {
                Some(bp) if bp >= min_bp => {
                    self.consume_token();
                    let rhs_min_bp = bp + 1;
                    let rhs = self.parse_expression(rhs_min_bp)?;

                    let operator = self
                        .token_to_operator(&operator_token)
                        .ok_or(ParserError::UnexpectedToken(operator_token))?;

                    lhs = Expression {
                            kind: ExpressionKind::BinaryOperation {
                                left: Box::new(
                                    Expression { kind: lhs.kind, type_: None }
                                ),
                                operator,
                                right: Box::new(rhs),
                                span
                            },
                            type_: None
                    };
                }
                _ => break,
            }
        }

        return Ok(lhs);
    }

}