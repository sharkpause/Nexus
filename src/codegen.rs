use std::{ collections::HashMap };

use crate::parser::{Expression, Operator, Statement, TopLevel, Type};

#[derive(Debug)]
pub enum CodegenError {
    GenericError,
    UndefinedVariable(String)
}

pub struct CodeGenerator {
    symbol_table: Vec<HashMap<String, i64>>,
    stack_size: i64
}

impl Default for CodeGenerator {
    fn default() -> Self {
        return Self {
            symbol_table: Vec::new(),
            stack_size: 8 // 8 so offset is always a multiple of 8
        };
    }
}

impl CodeGenerator {
    pub fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }
    
    pub fn exit_scope(&mut self) {
        self.symbol_table.pop().expect("Scope underflow");
    }

    pub fn lookup_variable(&self, name: &str) -> Result<i64, CodegenError> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(offset) = scope.get(name) {
                return Ok(*offset);
            }
        }
        
        return Err(CodegenError::UndefinedVariable(String::from(name)));
    }

    pub fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        let mut output = String::from(
            "global _start\n\
            _start:\n\
                \tcall entry\n\
                \tmov rdi, rax\n\
                \tmov rax, 60\n\
                \tsyscall\n\n"
        );

        for toplevel in program {
            match toplevel {
                TopLevel::Function(function) => {
                    self.stack_size = 8;
                    self.symbol_table.clear();

                    self.enter_scope();
                    
                    output.push_str(&format!("{}:\n", function.name));

                    let mut parameter_code = String::new();
                    if function.parameters.len() > 0 {
                        for (index, (type_, name)) in function.parameters.iter().enumerate() {
                            self.stack_size += 8;
                            let offset = self.stack_size - 8;

                            let current_scope = self.symbol_table.last_mut().expect("No active scope");

                            current_scope.insert(name.clone(), offset);

                            let register = match index {
                                0 => "rdi",
                                1 => "rsi",
                                2 => "rdx",
                                3 => "rcx",
                                4 => "r8",
                                5 => "r9",
                                _ => unimplemented!("Stack arguments not supported"),
                            };

                            parameter_code.push_str(&format!("\tmov [rbp - {}], {}\n", offset, register));
                        }
                    }

                    let statements_code = self.generate_function_body(function.body)?;

                    // this ordering may look confusing but it's needed so self.stack_size
                    // is the correct amount of bytes
                    output.push_str(&format!(
                        "\tpush rbp\n\
                        \tmov rbp, rsp\n\
                        \tsub rsp, {}\n",
                        self.stack_size));

                    output.push_str(&parameter_code);

                    output.push_str(&statements_code);

                    output.push_str(
                        "\tmov rsp, rbp\n\
                        \tpop rbp\n\
                        \tret\n"
                    );

                    self.exit_scope();
                },
                TopLevel::Statement(statement) => {
                    let statement_code = self.generate_statement(statement)?;

                    output.push_str(&statement_code);
                }
            }
        }

        return Ok(output);
    }

    fn generate_function_body(&mut self, statement: Statement) -> Result<String, CodegenError> {
        match statement {
            Statement::Block(statements) => {
                let mut statements_code = String::new();
                
                for statement in statements {
                    let statement_code = self.generate_statement(statement)?;
                    statements_code.push_str(&statement_code);
                }

                return Ok(statements_code);
            },
            _ => {
                return Err(CodegenError::GenericError)
            }
        }
    }

    fn generate_statement(&mut self, statement: Statement) -> Result<String, CodegenError> {
        let mut output = String::new();

        match statement {
            Statement::Block(statements) => {
                self.enter_scope();

                let mut statements_code = String::new();
                
                for statement in statements {
                    let statement_code = self.generate_statement(statement)?;
                    statements_code.push_str(&statement_code);
                }

                self.exit_scope();
                return Ok(statements_code);
            },
            Statement::Return(expression) => {
                output.push_str(&self.generate_expression(&expression)?);
            
                return Ok(output);
            },
            Statement::VariableDeclare(var_type, var_name, expression) => {
                let stack_offset;
                
                match var_type {
                    Type::Int => {
                        self.stack_size += 8;
                        stack_offset = self.stack_size - 8;
                    }
                }

                let current_scope = self.symbol_table.last_mut().expect("No active scope");

                current_scope.insert(format!("{}", var_name), stack_offset);

                output.push_str(&self.generate_expression(&expression)?);
                output.push_str(&format!("    mov [rbp - {}], rax\n", stack_offset));
                return Ok(output);
            },
            Statement::VariableAssignment(name, expression) => {
                let offset = self.lookup_variable(&name)?;
            
                output.push_str(&self.generate_expression(&expression)?);
                output.push_str(&format!("    mov [rbp - {}], rax\n", offset));

                return Ok(output);
            },
            Statement::Expression(expression) => {
                let output = self.generate_expression(&expression)?;

                return Ok(output);
            }
            _ => {
                return Err(CodegenError::GenericError);
            }
        }
    }

    fn generate_expression(&self, expression: &Expression) -> Result<String, CodegenError> {
        let mut output = String::new();

        match expression {
            Expression::FunctionCall(name, arguments) => {
                for (index, argument) in arguments.iter().enumerate() {
                    output.push_str(&self.generate_expression(argument)?);

                    let register = match index {
                        0 => "rdi",
                        1 => "rsi",
                        2 => "rdx",
                        3 => "rcx",
                        4 => "r8",
                        5 => "r9",
                        _ => unimplemented!("Stack arguments are not supported yet"),
                    };

                    output.push_str(&format!("\tmov {}, rax\n", register));
                }

                let function_name = match &**name {
                    Expression::Variable(name) => {
                        name
                    },
                    _ => {
                        return Err(CodegenError::GenericError);
                    }
                };

                output.push_str(&format!("\tcall {}\n", function_name));
            },
            Expression::Variable(name) => {
                let offset = self.lookup_variable(&name)?;

                output.push_str(&format!("    mov rax, [rbp - {}]\n", offset));
            },
            Expression::IntLiteral(value) => {
                output.push_str(&format!("    mov rax, {}\n", value));
            },
            Expression::UnaryOperation(operator, inner) => {
                output.push_str(&self.generate_expression(inner)?);
                output.push_str("    neg rax\n");
            },
            Expression::BinaryOperation(lhs,operator ,rhs ) => {
                let left = self.generate_expression(lhs)?;
                output.push_str(&left);
                output.push_str("    push rax\n");

                let right = self.generate_expression(rhs)?;
                output.push_str(&right);
                output.push_str("    pop rcx\n");

                match operator {
                    Operator::Add => {
                        output.push_str("    add rcx, rax\n");
                        output.push_str("    mov rax, rcx\n");
                    },
                    Operator::Subtract => {
                        output.push_str("    sub rcx, rax\n");
                        output.push_str("    mov rax, rcx\n");
                    },
                    Operator::Multiply => {
                        output.push_str("    imul rcx, rax\n");
                        output.push_str("    mov rax, rcx\n");
                    },
                    Operator::Divide => {
                        output.push_str("    xchg rax, rcx\n");
                        output.push_str("    xor rdx, rdx\n");
                        output.push_str("    idiv rcx\n");
                    }
                }
            }
        }

        return Ok(output);
    }

}