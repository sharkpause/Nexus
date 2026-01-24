use std::{ collections::HashMap };

use crate::parser::{Expression, Operator, Statement, TopLevel, Type};
use crate::backend::Backend;
use crate::backend::CodegenError;

struct FunctionContext {
    return_type: Type,
}

#[derive(Clone)]
struct VariableContext {
    ssa_name: String,
    var_type: Type
}

pub struct LLVMCodeGenerator {
    ssa_counter: usize,
    block_counter: usize,
    loop_stack: Vec<(String, String)>,
    symbol_table: Vec<HashMap<String, VariableContext>>,
    indent_level: usize,
    current_function: Option<FunctionContext>
}

impl Default for LLVMCodeGenerator {
    fn default() -> Self {
        return Self {
            ssa_counter: 1,
            block_counter: 0,
            loop_stack: Vec::new(),
            symbol_table: Vec::new(),
            indent_level: 0,
            current_function: None
        };
    }
}

impl LLVMCodeGenerator {
    fn indent(&self) -> String {
        return "\t".repeat(self.indent_level);
    }
    
    fn enter_scope(&mut self) {
        self.symbol_table.push(HashMap::new());
    }
    
    fn exit_scope(&mut self) {
        self.symbol_table.pop().expect("Scope underflow");
    }

    fn lookup_variable(&self, name: &str) -> Result<VariableContext, CodegenError> {
        for scope in self.symbol_table.iter().rev() {
            if let Some(variable_context) = scope.get(name) {
                return Ok(variable_context.clone());
            }
        }
        
        return Err(CodegenError::UndefinedVariable(String::from(name)));
    }

    fn add_variable(&mut self, name: &str, type_: Type, value: &str) -> Result<(), CodegenError> {
        let scope = self.symbol_table
            .last_mut()
            .expect("Semantic analysis guarantees there's always an active scope here");

        scope.insert(name.to_string(), VariableContext {
            ssa_name: value.to_string(),
            var_type: type_
        });
    
        return Ok(());
    }

    fn map_type(&self, type_: &Type) -> &'static str {
        match type_ {
            Type::Int64 => return "i64",
            Type::Int32 => return "i32",
            Type::Void => return "void",
            _ => unreachable!("Other types should not be allowed in semantic analysis")
        }
    }

    pub fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        let mut output = String::new();

        for toplevel in program {
            match toplevel {
                TopLevel::Function(function) => {
                    self.enter_scope();
                    self.current_function = Some(FunctionContext {
                        return_type: function.return_type.clone()
                    });
                    
                    output.push_str(&format!(
                        "{}define {} @{}(",
                        self.indent(), self.map_type(&function.return_type), function.name
                    ));

                    let mut parameter_code = String::new();
                    if function.parameters.len() > 0 {                        
                        for (index, (type_, name)) in function.parameters.iter().enumerate() {
                            let type_llvm = self.map_type(type_);
                            
                            if index > 0 {
                                output.push_str(", ");
                            }
                            
                            output.push_str(&format!(
                                "{} %{}",
                                type_llvm, name
                            ));

                            let alloca_name = format!("%{}.addr", name);

                            parameter_code.push_str(&format!(
                                "{}{} = alloca {}\n",
                                self.indent(), alloca_name, type_llvm
                            ));

                            parameter_code.push_str(&format!(
                                "{}store {} %{}, {}* {}\n",
                                self.indent(), type_llvm, name, type_llvm, alloca_name
                            ));

                            self.add_variable(name, type_.clone(), &alloca_name);
                        }
                    }
                    
                    output.push_str(") {\n");
                    output.push_str(&format!("{}entry:\n", self.indent()));

                    self.indent_level += 1;

                    output.push_str(&parameter_code);

                    // the first Block needs to be handled specially by this method since arguments
                    // need to be in the same scope as the body
                    output.push_str(&self.generate_function_body(function.body)?);

                    self.indent_level -= 1;
                    output.push_str(&format!("{}}}\n", self.indent()));

                    self.exit_scope();
                },

                TopLevel::Statement(statement) => {
                    unreachable!("Semantic analysis guarantees statement is not allowed at the top level.");
                }                
            }
        }
        return Ok(output);
    }

    fn generate_function_body(&mut self, statement: Statement) -> Result<String, CodegenError> {
        match statement {
            Statement::Block{ statements, span } => {
                let mut statements_code = String::new();
                
                for statement in statements {
                    let statement_code = self.generate_statement(statement)?;
                    statements_code.push_str(&statement_code);
                }

                return Ok(statements_code);
            },

            _ => {
                unreachable!("Function body should always be a block");
            }
        }
    }

    fn generate_statement(&mut self, statement: Statement) -> Result<String, CodegenError> {
        match statement {
            Statement::Block{ statements, span} => {
                self.enter_scope();

                let mut statements_code = String::new();
                
                for statement in statements {
                    let statement_code = self.generate_statement(statement)?;
                    statements_code.push_str(&statement_code);
                }

                self.exit_scope();
                return Ok(statements_code);
            },

            Statement::Return{ value: expression, span } => {
                let mut statement_code = String::new();

                let return_type = self.current_function.as_ref().expect("Function should be guaranteed to have a type").return_type.clone();
                if return_type.is_void() {
                    statement_code.push_str(&format!("{}ret void\n", self.indent()));
                    return Ok(statement_code);
                }

                let (expression_code, ssa_value) = match expression {
                    Some(expr) => {
                        self.generate_expression(&expr, Some(&return_type))?
                    }
                    None => {
                        // This should never happen if semantic analysis is correct
                        unreachable!("Non-void function must return a value");
                    }
                };

                statement_code.push_str(&expression_code);
                statement_code.push_str(&format!(
                    "{}ret {} {}\n",
                    self.indent(), self.map_type(&return_type), ssa_value
                ));

                return Ok(statement_code);
            },
            
            Statement::VariableDeclare{ var_type, name: var_name , initializer, span } => {
                let mut statement_code = String::new();
                
                let ssa_name = &format!("%{}.addr", var_name);
                statement_code.push_str(&format!(
                    "{}{} = alloca {}\n",
                    self.indent(), ssa_name, self.map_type(&var_type)
                ));

                let (expression_code, expression_ssa) = self.generate_expression(&initializer, Some(&var_type))?;

                statement_code.push_str(&expression_code);
                statement_code.push_str(&format!(
                    "{}store {} {}, {}* {}\n",
                    self.indent(), self.map_type(&var_type), expression_ssa, self.map_type(&var_type), ssa_name
                ));

                self.add_variable(&var_name, var_type, ssa_name);

                return Ok(statement_code);
            },
            
            Statement::VariableAssignment{ name, value, span } => {
                let mut statement_code = String::new();
                
                let var_context = self.lookup_variable(&name)?;

                let (expression_code, expression_ssa) =
                    self.generate_expression(&value, Some(&var_context.var_type))?;

                statement_code.push_str(&expression_code);
                statement_code.push_str(&format!(
                    "{}store {} {}, {}* {}\n",
                    self.indent(), self.map_type(&var_context.var_type), expression_ssa, self.map_type(&var_context.var_type), var_context.ssa_name
                ));

                return Ok(statement_code);
            },

            Statement::Expression{ expression, span } => {
                let output = self.generate_expression(&expression, None)?;

                return Ok(output.0);
            },

            // Statement::If{ condition: expression, then_branch: body, else_branch: else_ } => {
            //     let mut output = self.generate_expression(&expression)?;
                
            //     let endif_label = format!("_endif_{}", self.if_label_counter);
            //     self.if_label_counter += 1;
                
            //     let mut then_code = String::new();
            //     then_code.push_str("\tcmp rax, 0\n");

            //     let mut branch_code = String::new();
            //     let else_label = match else_ {
            //         Some(statement) => {
            //             let label = format!("_else{}", self.if_label_counter);
            //             branch_code.push_str(&format!("\t{}:\n", label));
            //             branch_code.push_str(&self.generate_statement(*statement)?);
            //             label
            //         },
            //         None => {
            //             branch_code.push_str(&format!("\t{}:\n", endif_label));
            //             endif_label
            //         }
            //     };

            //     then_code.push_str(&format!("\tje {}\n", else_label));
            //     then_code.push_str(&self.generate_statement(*body)?);

            //     output.push_str(&then_code);
            //     output.push_str(&branch_code);

            //     return Ok(output);
            // },
            // Statement::While { condition, body } => {
            //     let start_label = format!("_loop_start{}", self.loop_label_counter);
            //     let end_label = format!("_loop_end{}", self.loop_label_counter);
                
            //     self.loop_stack.push((start_label.clone(), end_label.clone()));

            //     self.loop_label_counter += 1;
                
            //     let mut output = format!("\t{}:\n", start_label);

            //     output.push_str(&self.generate_expression(&condition)?);
            //     output.push_str(&format!("\tcmp rax, 0\n\tje {}\n", end_label));
            //     output.push_str(&self.generate_statement(*body)?);
            //     output.push_str(&format!("\tjmp {}\n", start_label));
            //     output.push_str(&format!("\t{}:\n", end_label));

            //     self.loop_stack.pop();

            //     return Ok(output);
            // },
            // Statement::Break => {
            //     let loop_context = self.loop_stack.last().ok_or(CodegenError::InvalidBreak)?;
            //     return Ok(format!("\tjmp {}\n", loop_context.1));
            // },
            // Statement::Continue => {
            //     let loop_context = self.loop_stack.last().ok_or(CodegenError::InvalidContinue)?;
            //     return Ok(format!("\tjmp {}\n", loop_context.0));
            // },
            _ => {
                return Err(CodegenError::UnknownStatement);
            }
        }
    }

    fn generate_expression(&mut self, expression: &Expression, expected_type: Option<&Type>) -> Result<(String, String), CodegenError> {
        match expression {
            Expression::IntLiteral { value, span } => {
                let some_expected_type = expected_type.expect("An expected type must be passed at this point");
                let ssa_type = match some_expected_type {
                    Type::Int32 => "i32",
                    Type::Int64 => "i64",
                    _ => {
                        unreachable!("Semantic analysis guarantees correct typing")
                    }
                };
                
                let ssa = format!("%{}", self.ssa_counter);
                let code = format!(
                    "{}{} = add {} 0, {}\n",
                    self.indent(), ssa, ssa_type, value
                );
            
                self.ssa_counter += 1;
                return Ok((code, ssa));
            },

            Expression::Variable { name, type_, span } => {
                let variable_pointer = self.lookup_variable(name)?;
                let ssa_type = self.map_type(&variable_pointer.var_type);

                let ssa = format!("%{}", self.ssa_counter);
                let code = format!(
                    "{}{} = load {}, {}* {}\n",
                    self.indent(), ssa, ssa_type, ssa_type, variable_pointer.ssa_name
                );

                self.ssa_counter += 1;
            
                return Ok((code, ssa));
            },

            Expression::FunctionCall { called, arguments, span } => {
                let mut code = String::new();
                let mut argument_ssas: Vec<String> = Vec::new();

                for argument in arguments {
                    println!("\n\n{:?}\n\n", argument);
                    let generated_expression = self.generate_expression(argument, expected_type)?;
                
                    code.push_str(&generated_expression.0);
                    argument_ssas.push(generated_expression.1);
                }

                let Expression::Variable { name: function_name, type_: return_type, span } = called.as_ref()
                    else { unreachable!("Expression is guaranteed to be a variable") };

                let argument_code = argument_ssas
                    .into_iter()
                    .map(|ssa| format!("{}", ssa))
                    .collect::<Vec<_>>()
                    .join(", ");

                let function_call_ssa = format!("%{}", function_name);

                match return_type {
                    Some(type_) => {
                        code.push_str(&format!(
                            "{}{} = call {} @{}({})",
                            self.indent(), function_call_ssa, self.map_type(type_), function_name, argument_code
                        ));
                    },
                    None => unreachable!("Return type is guaranteed")
                }

                return Ok((code, function_call_ssa));
            }

            Expression::BinaryOperation { left, operator, right, span } => {
                let (left_code, left_ssa) = self.generate_expression(left, expected_type)?;
                let (right_code, right_ssa) = self.generate_expression(right, expected_type)?;

                let ssa = format!("%{}", self.ssa_counter);
                self.ssa_counter += 1;

                let operator_code = match operator {
                    Operator::Add => "add",
                    Operator::Subtract => "sub",
                    Operator::Multiply => "mul",
                    Operator::Divide => "sdiv",
                    _ => unimplemented!("Other operators not yet"),
                };

                let mut code = String::new();

                code.push_str(&left_code);
                code.push_str(&right_code);
                code.push_str(&format!(
                    "{}{} = {} i64 {}, {}\n",
                    self.indent(),
                    ssa,
                    operator_code,
                    left_ssa,
                    right_ssa
                ));

                return Ok((code, ssa));
            },

            Expression::UnaryOperation { operator, operand, span } => {
                todo!("implement this shit");
            },

            Expression::IntLiteral32 { value, span } => {
                todo!("Implement ts");
            },

            Expression::IntLiteral64 { value, span } => {
                todo!("Implement ts");
            },

            // _ => {
            //     return Err(CodegenError::UnknownExpression);
            // }
        }
        
        // let mut output = String::new();

        // match expression {
        //     Expression::FunctionCall{ callee: name, arguments } => {
        //         for (index, argument) in arguments.iter().enumerate() {
        //             output.push_str(&self.generate_expression(argument)?);

        //             let register = match index {
        //                 0 => "rdi",
        //                 1 => "rsi",
        //                 2 => "rdx",
        //                 3 => "rcx",
        //                 4 => "r8",
        //                 5 => "r9",
        //                 _ => unimplemented!("Stack arguments are not supported yet"),
        //             };

        //             output.push_str(&format!("\tmov {}, rax\n", register));
        //         }

        //         let function_name = match &**name {
        //             Expression::Variable{ name } => {
        //                 name
        //             },
        //             _ => {
        //                 return Err(CodegenError::GenericError);
        //             }
        //         };

        //         output.push_str(&format!("\tcall {}\n", function_name));
        //     },
        //     Expression::Variable{ name } => {
        //         let offset = self.lookup_variable(&name)?;

        //         output.push_str(&format!("\tmov rax, [rbp - {}]\n", offset));
        //     },
        //     Expression::IntLiteral{ value } => {
        //         output.push_str(&format!("\tmov rax, {}\n", value));
        //     },
        //     Expression::UnaryOperation{ operator, operand: inner } => {
        //         output.push_str(&self.generate_expression(inner)?);
        //         output.push_str("\tneg rax\n");
        //     },
        //     Expression::BinaryOperation{ left: lhs,operator ,right: rhs } => {
        //         let left = self.generate_expression(lhs)?;
        //         output.push_str(&left);
        //         output.push_str("\tpush rax\n");

        //         let right = self.generate_expression(rhs)?;
        //         output.push_str(&right);
        //         output.push_str("\tpop rcx\n");

        //         match operator {
        //             Operator::Add => {
        //                 output.push_str("\tadd rcx, rax\n");
        //                 output.push_str("\tmov rax, rcx\n");
        //             },
        //             Operator::Subtract => {
        //                 output.push_str("\tsub rcx, rax\n");
        //                 output.push_str("\tmov rax, rcx\n");
        //             },
        //             Operator::Multiply => {
        //                 output.push_str("\timul rcx, rax\n");
        //                 output.push_str("\tmov rax, rcx\n");
        //             },
        //             Operator::Divide => {
        //                 output.push_str("\txchg rax, rcx\n");
        //                 output.push_str("\txor rdx, rdx\n");
        //                 output.push_str("\tidiv rcx\n");
        //             },
        //             Operator::Equal => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsete al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::NotEqual => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsetne al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::LessThan => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsetl al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::LessEqual => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsetle al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::GreaterThan => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsetg al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::GreaterEqual => {
        //                 output.push_str("\tcmp rcx, rax\n");
        //                 output.push_str("\tsetge al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::And => {
        //                 output.push_str("\tcmp rcx, 0\n");
        //                 output.push_str("\tsetne cl\n");
        //                 output.push_str("\tcmp rax, 0\n");
        //                 output.push_str("\tsetne al\n");
        //                 output.push_str("\tand al, cl\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::Or => {
        //                 output.push_str("\tcmp rcx, 0\n");
        //                 output.push_str("\tsetne cl\n");
        //                 output.push_str("\tcmp rax, 0\n");
        //                 output.push_str("\tsetne al\n");
        //                 output.push_str("\tor al, cl\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             Operator::Not => {
        //                 output.push_str("\tcmp rax, 0\n");
        //                 output.push_str("\tsete al\n");
        //                 output.push_str("\tmovzx rax, al\n");
        //             },
        //             _ => {
        //                 return Err(CodegenError::GenericError);
        //             }
        //         }
        //     }
        // }

        // return Ok(output);
    }
}

impl Backend for LLVMCodeGenerator {
    fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        return self.generate(program);
    }
}