use std::collections::HashMap;

use crate::parser::{Expression, Operator, Statement, TopLevel, Type};

#[derive(Debug)]
pub enum CodegenError {
    GenericError
}

pub struct CodeGenerator {
    symbol_table: HashMap<String, i64>,
    stack_size: i64
}

impl Default for CodeGenerator {
    fn default() -> Self {
        return Self {
            symbol_table: HashMap::new(),
            stack_size: 0
        };
    }
}

impl CodeGenerator {

    pub fn generate(&self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
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
                    output.push_str(&format!("{}:\n", function.name));

                    for statement in function.body {
                        let statement_code = self.generate_statement(statement)?;

                        output.push_str(&statement_code);
                    }
                },
                TopLevel::Statement(statement) => {
                    let statement_code = self.generate_statement(statement)?;

                    output.push_str(&statement_code);
                }
            }
        }

        return Ok(output);
    }

    fn generate_statement(&self, statement: Statement) -> Result<String, CodegenError> {
        let mut output = String::new();

        match statement {
            Statement::Return(expression) => {
                output.push_str(&self.generate_expression(expression)?);
                output.push_str("    ret\n");
            
                return Ok(output);
            },
            // Statement::VariableDeclare(var_type, var_name, expression) {

            // },
            _ => {
                return Err(CodegenError::GenericError);
            }
        }
    }

    fn generate_expression(&self, expression: Expression) -> Result<String, CodegenError> {
        let mut output = String::new();

        match expression {
            Expression::IntLiteral(value) => {
                output.push_str(&format!("    mov rax, {}\n", value));
            },
            Expression::UnaryOp(operator, inner) => {
                output.push_str(&self.generate_expression(*inner)?);
                output.push_str("    neg rax\n");
            },
            Expression::BinaryOp(lhs,operator ,rhs ) => {
                let left = self.generate_expression(*lhs)?;
                output.push_str(&left);
                output.push_str("    push rax\n");

                let right = self.generate_expression(*rhs)?;
                output.push_str(&right);
                output.push_str("    pop rbx\n");

                match operator {
                    Operator::Add => {
                        output.push_str("    add rbx, rax\n");
                        output.push_str("    mov rax, rbx\n");
                    },
                    Operator::Subtract => {
                        output.push_str("    sub rbx, rax\n");
                        output.push_str("    mov rax, rbx\n");
                    },
                    Operator::Multiply => {
                        output.push_str("    imul rbx, rax\n");
                        output.push_str("    mov rax, rbx\n");
                    },
                    Operator::Divide => {
                        output.push_str("    xchg rax, rbx\n");
                        output.push_str("    xor rdx, rdx\n");
                        output.push_str("    idiv rbx\n");
                    }
                }
            }
        }

        return Ok(output);
    }

}