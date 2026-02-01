use std::fmt::format;
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
    // block_counter: usize,
    loop_stack: Vec<(String, String)>,
    symbol_table: Vec<HashMap<String, VariableContext>>,
    indent_level: usize,
    current_function: Option<FunctionContext>,
    if_label_counter: usize,
    loop_label_counter: usize,
    globals_code: String,
}

impl Default for LLVMCodeGenerator {
    fn default() -> Self {
        return Self {
            ssa_counter: 1,
            // block_counter: 0,
            loop_stack: Vec::new(),
            symbol_table: Vec::new(),
            indent_level: 0,
            current_function: None,
            if_label_counter: 0,
            loop_label_counter: 0,
            globals_code: String::new()
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
            Type::String => return "i8*",
            _ => unreachable!("Other types should not be allowed in semantic analysis")
        }
    }

    fn infer_expression_type(&mut self, expression: &Expression) -> Type {
        return match expression {
            Expression::IntLiteral { .. } => Type::GenericInt, // Might change this to either panic or unreachable
            Expression::IntLiteral32 { .. } => Type::Int32,
            Expression::IntLiteral64 { .. } => Type::Int64,
            Expression::StringLiteral { .. } => Type::String,

            Expression::Variable { type_, .. } => {
                type_.clone().expect("Variable must have type from semantic analysis")
            },

            Expression::BinaryOperation { left, right, .. } => {
                let left_type = self.infer_expression_type(left);
                let right_type = self.infer_expression_type(right);

                if left_type.same_kind(&right_type) {
                    left_type
                } else {
                    panic!("Mismatched types in codegen, should have been caught in semantic analysis");
                }
            }

            Expression::UnaryOperation { operand, .. } => self.infer_expression_type(operand),

            Expression::FunctionCall { called, .. } => {
                let Expression::Variable { type_, .. } = called.as_ref()
                    else { unreachable!("Function call must have a variable callee") };
                type_.clone().expect("Function must have return type from semantic analysis")
            },

            _ => panic!("Expression type inference not implemented for this variant"),
        };
    }

    pub fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        let mut final_output = String::new();
        let mut output = String::new();

        // Below is temporary to hardcode the puts function from C
        output.push_str("declare i32 @puts(i8*)\n\n");

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
                        parameter_code.push_str("\n");
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

        final_output.push_str(&self.globals_code);
        final_output.push_str(&output);

        return Ok(final_output);
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

            Statement::If { condition, then_branch, else_branch, span } => {
                let mut code = String::new();

                let then_label = format!("_if_then_{}", self.if_label_counter);
                let else_label = format!("_if_else_{}", self.if_label_counter);
                let endif_label = format!("_if_end_{}", self.if_label_counter);
                self.if_label_counter += 1;

                let (cond_code, cond_ssa) = self.generate_expression(&condition, Some(&Type::Int32))?;
                code.push_str(&cond_code);

                let cond_i1_ssa = format!("%{}", self.ssa_counter);
                self.ssa_counter += 1;
                code.push_str(&format!(
                    "{}{} = icmp ne i32 {}, 0\n",
                    self.indent(),
                    cond_i1_ssa,
                    cond_ssa
                ));

                let else_target = if else_branch.is_some() { &else_label } else { &endif_label };
                code.push_str(&format!(
                    "{}br i1 {}, label %{}, label %{}\n",
                    self.indent(),
                    cond_i1_ssa,
                    then_label,
                    else_target
                ));

                code.push_str(&format!("{}:\n", then_label));
                let then_code = self.generate_statement(*then_branch)?;
                code.push_str(&then_code);

                // ends_with_terminator is needed to prevent double "ret"s and "br"s that
                // confuses LLVM
                let then_ends_with_terminator = then_code
                    .lines()
                    .rev()
                    .find(|line| !line.trim().is_empty())
                    .map(|line| {
                        let line = line.trim_start();
                        line.starts_with("ret") || line.starts_with("br")
                    })
                    .unwrap_or(false);

                if !then_ends_with_terminator {
                    code.push_str(&format!("{}br label %{}\n", self.indent(), endif_label));
                }

                if let Some(else_stmt) = else_branch {
                    code.push_str(&format!("{}:\n", else_label));

                    let else_code = self.generate_statement(*else_stmt)?;
                    code.push_str(&else_code);
                    
                    let else_ends_with_terminator = else_code
                        .lines()
                        .rev()
                        .find(|line| !line.trim().is_empty())
                        .map(|line| {
                            let line = line.trim_start();
                            line.starts_with("ret") || line.starts_with("br")
                        })
                        .unwrap_or(false);

                    if !else_ends_with_terminator {
                        code.push_str(&format!("{}br label %{}\n", self.indent(), endif_label));
                    }
                }

                code.push_str(&format!("{}:\n", endif_label));

                return Ok(code);
            }

            Statement::While { condition, body, span } => {
                let mut code = String::new();

                let cond_label = format!("_while_cond_{}", self.loop_label_counter);
                let body_label = format!("_while_body_{}", self.loop_label_counter);
                let end_label = format!("_while_end_{}", self.loop_label_counter);
                self.loop_label_counter += 1;

                self.loop_stack.push((cond_label.clone(), end_label.clone()));

                code.push_str(&format!("{}br label %{}\n", self.indent(), cond_label));

                code.push_str(&format!("{}:\n", cond_label));
                let (cond_code, cond_ssa) = self.generate_expression(&condition, Some(&Type::Int32))?;
                code.push_str(&cond_code);

                let cond_i1_ssa = format!("%{}", self.ssa_counter);
                self.ssa_counter += 1;
                code.push_str(&format!(
                    "{}{} = icmp ne i32 {}, 0\n",
                    self.indent(),
                    cond_i1_ssa,
                    cond_ssa
                ));

                code.push_str(&format!(
                    "{}br i1 {}, label %{}, label %{}\n",
                    self.indent(),
                    cond_i1_ssa,
                    body_label,
                    end_label
                ));

                code.push_str(&format!("{}:\n", body_label));
                code.push_str(&self.generate_statement(*body)?);
                code.push_str(&format!("{}br label %{}\n", self.indent(), cond_label));

                code.push_str(&format!("{}:\n", end_label));

                self.loop_stack.pop();

                return Ok(code);
            }

            Statement::Break { span } => {
                if let Some((_, end_label)) = self.loop_stack.last() {
                    return Ok(format!("{}br label %{}\n", self.indent(), end_label));
                } else {
                    return Err(CodegenError::InvalidBreak);
                }
            }

            Statement::Continue { span } => {
                if let Some((cond_label, _)) = self.loop_stack.last() {
                    return Ok(format!("{}br label %{}\n", self.indent(), cond_label));
                } else {
                    return Err(CodegenError::InvalidContinue);
                }
            }

            // _ => {
            //     return Err(CodegenError::UnknownStatement);
            // }
        }
    }

    fn generate_expression(&mut self, expression: &Expression, expected_type: Option<&Type>) -> Result<(String, String), CodegenError> {
        match expression {
            Expression::IntLiteral { value, span } => {
                // let some_expected_type = expected_type.expect("An expected type must be passed at this point");
                // let ssa_type = match some_expected_type {
                //     Type::Int32 => "i32",
                //     Type::Int64 => "i64",
                //     _ => {
                //         unreachable!("Semantic analysis guarantees correct typing")
                //     }
                // };
                
                // let ssa = format!("%{}", self.ssa_counter);
                // let code = format!(
                //     "{}{} = add {} 0, {}\n",
                //     self.indent(), ssa, ssa_type, value
                // );
            
                // self.ssa_counter += 1;
                // return Ok((code, ssa));

                // Invalid type because semantic analysis should eliminate all generics
                return Err(CodegenError::InvalidType);
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
                let mut argument_types: Vec<Type> = Vec::new();

                for argument in arguments {
                    let generated_expression = self.generate_expression(argument, expected_type)?;
                    argument_types.push(
                        self.infer_expression_type(argument)
                    );
                
                    code.push_str(&generated_expression.0);
                    argument_ssas.push(generated_expression.1);
                    
                }

                let Expression::Variable { name: function_name, type_: return_type, span } = called.as_ref()
                    else { unreachable!("Expression is guaranteed to be a variable") };

                let argument_code = argument_types
                    .iter()
                    .zip(argument_ssas)
                    .map(
                        |(arg_type, arg_ssa)|
                        format!("{} {}", self.map_type(arg_type), arg_ssa)
                    )
                    .collect::<Vec<_>>()
                    .join(", ");

                let function_call_ssa = format!("%{}{}", function_name, self.ssa_counter);
                self.ssa_counter += 1;

                match return_type {
                    Some(type_) => {
                        if type_.is_void() {
                            code.push_str(&format!(
                                "{}call {} @{}({})\n",
                                self.indent(), self.map_type(type_), function_name, argument_code
                            ));
                        } else {
                            code.push_str(&format!(
                                "{}{} = call {} @{}({})\n",
                                self.indent(), function_call_ssa, self.map_type(type_), function_name, argument_code
                            ));
                        }
                    },
                    None => unreachable!("Return type is guaranteed")
                }

                return Ok((code, function_call_ssa));
            }

            Expression::BinaryOperation { left, operator, right, span } => {
                let (left_code, left_ssa) = self.generate_expression(left, expected_type)?;
                let (right_code, right_ssa) = self.generate_expression(right, expected_type)?;

                let mut code = String::new();
                code.push_str(&left_code);
                code.push_str(&right_code);

                let mut ssa: String;

                match operator {
                    // Arithmetic
                    Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
                        let op_str = match operator {
                            Operator::Add => "add",
                            Operator::Subtract => "sub",
                            Operator::Multiply => "mul",
                            Operator::Divide => "sdiv",
                            _ => unreachable!(),
                        };
                        
                        ssa = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;
                        
                        let ssa_type = expected_type.expect("Semantic analysis should guarantee a type here");
                        
                        code.push_str(&format!(
                            "{}{} = {} {} {}, {}\n",
                            self.indent(),
                            ssa,
                            op_str,
                            self.map_type(ssa_type),
                            left_ssa,
                            right_ssa
                        ));
                    }

                    // Comparisons
                    Operator::Equal | Operator::NotEqual | Operator::LessThan
                    | Operator::GreaterThan | Operator::LessEqual | Operator::GreaterEqual => {
                        let cmp_str = match operator {
                            Operator::Equal => "eq",
                            Operator::NotEqual => "ne",
                            Operator::LessThan => "slt",
                            Operator::GreaterThan => "sgt",
                            Operator::LessEqual => "sle",
                            Operator::GreaterEqual => "sge",
                            _ => unreachable!(),
                        };

                        let cmp_ssa = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;
                        code.push_str(&format!(
                            "{}{} = icmp {} i32 {}, {}\n",
                            self.indent(),
                            cmp_ssa,
                            cmp_str,
                            left_ssa,
                            right_ssa
                        ));

                        ssa = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;
                        
                        if let Some(expected) = expected_type {
                            // Converts i1 to expected type in cases where the result is stored
                            // or returned.

                            code.push_str(&format!(
                                "{}{} = zext i1 {} to {}\n",
                                self.indent(),
                                ssa,
                                cmp_ssa,
                                self.map_type(expected)
                            ));
                        } else {
                            // If no type are expected, no conversion needs to happen
                            ssa = cmp_ssa;
                        }
                    }

                    // Logical operators
                    Operator::And | Operator::Or => {
                        // Convert operands to i1 first since LLVM needs i1 for both operands
                        // for logical operators.

                        let left_bool = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;
                        code.push_str(&format!(
                            "{}{} = icmp ne i32 {}, 0\n",
                            self.indent(),
                            left_bool,
                            left_ssa
                        ));

                        let right_bool = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;
                        code.push_str(&format!(
                            "{}{} = icmp ne i32 {}, 0\n",
                            self.indent(),
                            right_bool,
                            right_ssa
                        ));

                        ssa = format!("%{}", self.ssa_counter);
                        self.ssa_counter += 1;

                        let op_str = match operator {
                            Operator::And => "and",
                            Operator::Or => "or",
                            _ => unreachable!(),
                        };

                        code.push_str(&format!(
                            "{}{} = {} i1 {}, {}\n",
                            self.indent(),
                            ssa,
                            op_str,
                            left_bool,
                            right_bool
                        ));

                        if let Some(expected) = expected_type {
                            let zext_ssa = format!("%{}", self.ssa_counter);
                            self.ssa_counter += 1;
                            code.push_str(&format!(
                                "{}{} = zext i1 {} to {}\n",
                                self.indent(),
                                zext_ssa,
                                ssa,
                                self.map_type(expected)
                            ));
                            ssa = zext_ssa;
                        }
                    }

                    _ => unimplemented!("Other operators not yet"),
                }

                return Ok((code, ssa));
            }

            Expression::UnaryOperation { operator, operand, span } => {
                let (operand_code, operand_ssa) = self.generate_expression(operand, expected_type)?;
                let operand_type = self.infer_expression_type(operand);

                let ssa = format!("%{}", self.ssa_counter);
                self.ssa_counter += 1;

                let code = match operator {
                    Operator::Subtract => {
                        format!(
                            "{}\n{}{} = sub {} 0, {}\n",
                            operand_code, 
                            self.indent(),
                            ssa,
                            self.map_type(&operand_type),
                            operand_ssa
                        )
                    },
                    
                    Operator::Not => {
                        let tmp_ssa = format!("%tmp{}", self.ssa_counter);
                        self.ssa_counter += 1;

                        format!(
                            "{}\n{}{} = icmp eq {} {}, 0\n{}{} = zext i1 {} to {}\n",
                            operand_code,
                            self.indent(), tmp_ssa, self.map_type(&operand_type), operand_ssa,
                            self.indent(), ssa, tmp_ssa, self.map_type(&operand_type)
                        )
                    },
                    _ => unimplemented!("Other unary operators not implemented yet"),
                };

                return Ok((code, ssa));
            }

            Expression::IntLiteral32 { value, span } => {
                let ssa = format!("%{}", self.ssa_counter);
                let code = format!(
                    "{}{} = add i32 0, {}\n",
                    self.indent(), ssa, value
                );
                self.ssa_counter += 1;

                return Ok((code, ssa));
            },

            Expression::IntLiteral64 { value, span } => {
                let ssa = format!("%{}", self.ssa_counter);
                let code = format!(
                    "{}{} = add i64 0, {}\n",
                    self.indent(), ssa, value
                );
                self.ssa_counter += 1;

                return Ok((code, ssa));
            },

            Expression::StringLiteral { value, span } => {
                let string_literal_name = format!("@.str_{}", self.ssa_counter);
                self.ssa_counter += 1;

                let string_literal_length = value.len() + 1;

                self.globals_code.push_str(&format!(
                    "{} = private unnamed_addr constant [{} x i8] c\"{}\\00\"\n",
                    string_literal_name, string_literal_length, value
                ));

                let ssa = format!("%{}", self.ssa_counter);
                self.ssa_counter += 1;

                let code = format!(
                    "{}{} = getelementptr [{} x i8], [{} x i8]* {}, i32 0, i32 0\n",
                    self.indent(), ssa, string_literal_length, string_literal_length, string_literal_name
                );

                return Ok((code, ssa));
            }
        }
    }
}

impl Backend for LLVMCodeGenerator {
    fn generate(&mut self, program: Vec<TopLevel>) -> Result<String, CodegenError> {
        return self.generate(program);
    }
}