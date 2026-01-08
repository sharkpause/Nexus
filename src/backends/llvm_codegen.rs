use std::{ collections::HashMap };

use crate::parser::{Expression, Operator, Statement, TopLevel, Type};
use crate::backend::Backend;
use crate::backend::CodegenError;

pub struct LLVMCodeGenerator {
    symbol_table: Vec<HashMap<String, i64>>,
    stack_size: i64,
    if_label_counter: i64,
    loop_label_counter: i64,
    loop_stack: Vec<(String, String)>
}

impl Default for LLVMCodeGenerator {
    fn default() -> Self {
        return Self {
            symbol_table: Vec::new(),
            stack_size: 8, // 8 so offset is always a multiple of 8
            if_label_counter: 0,
            loop_label_counter: 0,
            loop_stack: Vec::new()
        };
    }
}

impl LLVMCodeGenerator {
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
            Statement::Block{ statements } => {
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
            Statement::Block{ statements} => {
                self.enter_scope();

                let mut statements_code = String::new();
                
                for statement in statements {
                    let statement_code = self.generate_statement(statement)?;
                    statements_code.push_str(&statement_code);
                }

                self.exit_scope();
                return Ok(statements_code);
            },
            Statement::Return{ value: expression } => {
                output.push_str(&self.generate_expression(&expression)?);
                output.push_str(
                    "\tmov rsp, rbp\n\
                    \tpop rbp\n\
                    \tret\n"
                );
            
                return Ok(output);
            },
            Statement::VariableDeclare{ var_type, name: var_name , initializer: expression } => {
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
                output.push_str(&format!("\tmov [rbp - {}], rax\n", stack_offset));
                return Ok(output);
            },
            Statement::VariableAssignment{ name, value: expression } => {
                let offset = self.lookup_variable(&name)?;
            
                output.push_str(&self.generate_expression(&expression)?);
                output.push_str(&format!("\tmov [rbp - {}], rax\n", offset));

                return Ok(output);
            },
            Statement::Expression{ expression } => {
                let output = self.generate_expression(&expression)?;

                return Ok(output);
            },
            Statement::If{ condition: expression, then_branch: body, else_branch: else_ } => {
                let mut output = self.generate_expression(&expression)?;
                
                let endif_label = format!("_endif_{}", self.if_label_counter);
                self.if_label_counter += 1;
                
                let mut then_code = String::new();
                then_code.push_str("\tcmp rax, 0\n");

                let mut branch_code = String::new();
                let else_label = match else_ {
                    Some(statement) => {
                        let label = format!("_else{}", self.if_label_counter);
                        branch_code.push_str(&format!("\t{}:\n", label));
                        branch_code.push_str(&self.generate_statement(*statement)?);
                        label
                    },
                    None => {
                        branch_code.push_str(&format!("\t{}:\n", endif_label));
                        endif_label
                    }
                };

                then_code.push_str(&format!("\tje {}\n", else_label));
                then_code.push_str(&self.generate_statement(*body)?);

                output.push_str(&then_code);
                output.push_str(&branch_code);

                return Ok(output);
            },
            Statement::While { condition, body } => {
                let start_label = format!("_loop_start{}", self.loop_label_counter);
                let end_label = format!("_loop_end{}", self.loop_label_counter);
                
                self.loop_stack.push((start_label.clone(), end_label.clone()));

                self.loop_label_counter += 1;
                
                let mut output = format!("\t{}:\n", start_label);

                output.push_str(&self.generate_expression(&condition)?);
                output.push_str(&format!("\tcmp rax, 0\n\tje {}\n", end_label));
                output.push_str(&self.generate_statement(*body)?);
                output.push_str(&format!("\tjmp {}\n", start_label));
                output.push_str(&format!("\t{}:\n", end_label));

                self.loop_stack.pop();

                return Ok(output);
            },
            Statement::Break => {
                let loop_context = self.loop_stack.last().ok_or(CodegenError::InvalidBreak)?;
                return Ok(format!("\tjmp {}\n", loop_context.1));
            },
            Statement::Continue => {
                let loop_context = self.loop_stack.last().ok_or(CodegenError::InvalidContinue)?;
                return Ok(format!("\tjmp {}\n", loop_context.0));
            },
            _ => {
                return Err(CodegenError::GenericError);
            }
        }
    }

    fn generate_expression(&self, expression: &Expression) -> Result<String, CodegenError> {
        let mut output = String::new();

        match expression {
            Expression::FunctionCall{ callee: name, arguments } => {
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
                    Expression::Variable{ name } => {
                        name
                    },
                    _ => {
                        return Err(CodegenError::GenericError);
                    }
                };

                output.push_str(&format!("\tcall {}\n", function_name));
            },
            Expression::Variable{ name } => {
                let offset = self.lookup_variable(&name)?;

                output.push_str(&format!("\tmov rax, [rbp - {}]\n", offset));
            },
            Expression::IntLiteral{ value } => {
                output.push_str(&format!("\tmov rax, {}\n", value));
            },
            Expression::UnaryOperation{ operator, operand: inner } => {
                output.push_str(&self.generate_expression(inner)?);
                output.push_str("\tneg rax\n");
            },
            Expression::BinaryOperation{ left: lhs,operator ,right: rhs } => {
                let left = self.generate_expression(lhs)?;
                output.push_str(&left);
                output.push_str("\tpush rax\n");

                let right = self.generate_expression(rhs)?;
                output.push_str(&right);
                output.push_str("\tpop rcx\n");

                match operator {
                    Operator::Add => {
                        output.push_str("\tadd rcx, rax\n");
                        output.push_str("\tmov rax, rcx\n");
                    },
                    Operator::Subtract => {
                        output.push_str("\tsub rcx, rax\n");
                        output.push_str("\tmov rax, rcx\n");
                    },
                    Operator::Multiply => {
                        output.push_str("\timul rcx, rax\n");
                        output.push_str("\tmov rax, rcx\n");
                    },
                    Operator::Divide => {
                        output.push_str("\txchg rax, rcx\n");
                        output.push_str("\txor rdx, rdx\n");
                        output.push_str("\tidiv rcx\n");
                    },
                    Operator::Equal => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsete al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::NotEqual => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsetne al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::LessThan => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsetl al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::LessEqual => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsetle al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::GreaterThan => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsetg al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::GreaterEqual => {
                        output.push_str("\tcmp rcx, rax\n");
                        output.push_str("\tsetge al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::And => {
                        output.push_str("\tcmp rcx, 0\n");
                        output.push_str("\tsetne cl\n");
                        output.push_str("\tcmp rax, 0\n");
                        output.push_str("\tsetne al\n");
                        output.push_str("\tand al, cl\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::Or => {
                        output.push_str("\tcmp rcx, 0\n");
                        output.push_str("\tsetne cl\n");
                        output.push_str("\tcmp rax, 0\n");
                        output.push_str("\tsetne al\n");
                        output.push_str("\tor al, cl\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    Operator::Not => {
                        output.push_str("\tcmp rax, 0\n");
                        output.push_str("\tsete al\n");
                        output.push_str("\tmovzx rax, al\n");
                    },
                    _ => {
                        return Err(CodegenError::GenericError);
                    }
                }
            }
        }

        return Ok(output);
    }

}

impl Backend for LLVMCodeGenerator {
    fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        return self.generate(program);
    }
}