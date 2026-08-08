mod lexing;
mod lexer;
mod parser;
// mod backend;
// mod backends;
mod semantic_analyzerv2;
mod parsing;
mod semantics;
mod errors;

use std::{env, fs, process::Command};

use crate::errors::{SemanticError, ParserError};
use crate::parser::Parser;
use crate::parsing::expression::{Expression, ExpressionKind};
use crate::parsing::statement::Statement;
use crate::parsing::toplevel::TopLevel;
use crate::semantic_analyzerv2::SemanticAnalyzer;
use crate::semantics::diagnostics::Diagnostics;
use crate::lexing::token::print_token;
use crate::lexer::Lexer;
// use crate::parsing::{ Parser, TopLevel, Statement, Expression, ParserError };
// use crate::backend::generate_program;
// use crate::backends::asm_codegen::ASMCodeGenerator;
// use crate::backends::llvm_codegen::LLVMCodeGenerator;

fn read_file(path: &String) -> String {
    let source_code =
        fs::read_to_string(path).expect("Failed to read source code");

    return source_code;
}

fn write_file(path: String, contents: &String) {
    fs::write(path, contents).expect("Failed to write assembly");
}

fn assemble_and_link(asm_path: &str, output_exe: &str) {
    let nasm_status = Command::new("nasm")
        .args(&["-f", "elf64", asm_path, "-o", "out.o"])
        .status()
        .expect("Failed to run NASM");

    if !nasm_status.success() {
        panic!("Assembling failed");
    }

    let ld_status = Command::new("ld")
        .args(&["out.o", "-o", output_exe])
        .status()
        .expect("Failed to run LD");

    if !ld_status.success() {
        panic!("Linking failed");
    }

    println!("Executable '{}' produced", output_exe);
}

fn print_statement(stmt: &Statement, indent: usize) {
    let padding = "  ".repeat(indent);

    match stmt {
        Statement::Return { value, span } => {
            println!("{}Return:", padding);
            if let Some(value_expression) = value {
                print_expression(value_expression, indent + 1);
            } else {
                println!("{}Void type", padding);
            }
        }

        Statement::VariableInitialize {
            var_type,
            name,
            initializer,
            span
        } => {
            println!("{}Initialize {:?} {}", padding, var_type, name);
            print_expression(initializer, indent + 1);
        }

        Statement::VariableAssignment { name, value, span } => {
            println!("{}Assign {}", padding, name);
            print_expression(value, indent + 1);
        }

        Statement::Block { statements, span } => {
            println!("{}Block:", padding);
            for stmt in statements {
                print_statement(stmt, indent + 1);
            }
        }

        Statement::Expression { expression, span } => {
            println!("{}Expression:", padding);
            print_expression(expression, indent + 1);
        }

        Statement::If {
            condition,
            then_branch,
            else_branch,
            span
        } => {
            println!("{}If:", padding);
            print_expression(condition, indent + 1);

            println!("{}Then:", padding);
            print_statement(then_branch, indent + 1);

            if let Some(else_stmt) = else_branch {
                println!("{}Else:", padding);
                print_statement(else_stmt, indent + 1);
            }
        }

        Statement::While { condition, body, span } => {
            println!("{}While:", padding);
            print_expression(condition, indent + 1);
            print_statement(body, indent + 1);
        }

        Statement::Break { span } => {
            println!("{}Break", padding);
        }

        Statement::Continue { span } => {
            println!("{}Continue", padding);
        }
    }
}


fn print_expression(expr: &Expression, indent: usize) {
    let padding = "  ".repeat(indent);

    print!("{} {:?} ", padding, expr.type_);

    match &expr.kind {
        ExpressionKind::Variable { name } => {
            println!("Variable {}", name);
        }

        ExpressionKind::IntLiteral { value } => {
            println!("Int {}", value);
        }

        ExpressionKind::UnaryOperation { operator, operand } => {
            println!("Unary {:?}", operator);
            print_expression(operand, indent + 1);
        }

        ExpressionKind::BinaryOperation {
            left,
            operator,
            right,
        } => {
            println!("Binary {:?}", operator);
            print_expression(left, indent + 1);
            print_expression(right, indent + 1);
        }

        ExpressionKind::FunctionCall {
            called,
            arguments,
        } => {
            println!("Call:");
            print_expression(called, indent + 1);

            for argument in arguments {
                print_expression(argument, indent + 1);
            }
        }

        ExpressionKind::StringLiteral { value } => {
            println!("String literal: \"{}\"", value);
        }

        ExpressionKind::BooleanLiteral { value } => {
            println!("Boolean literal: {}", value);
        }

        ExpressionKind::NullLiteral => {
            println!("Null literal");
        }
    }
}

pub fn print_semantic_errors(diagnostics: &Diagnostics) {
    for error in diagnostics.errors.iter() {
        match error {
            SemanticError::NoEntryFunction => {
                eprintln!("Semantic error: no 'entry' function found");
            }

            SemanticError::MainIsReserved { span } => {
                eprintln!(
                    "Semantic error at {}:{}: 'main' is a reserved function name",
                    span.line, span.column
                );
            }

            SemanticError::DuplicateVariable { name, span } => {
                eprintln!(
                    "Semantic error at {}:{}: duplicate variable '{}'",
                    span.line, span.column, name
                );
            }

            SemanticError::DuplicateFunction { name, span } => {
                eprintln!(
                    "Semantic error at {}:{}: duplicate function '{}'",
                    span.line, span.column, name
                );
            }

            SemanticError::DuplicateParameter { name, span } => {
                eprintln!(
                    "Semantic error at {}:{}: duplicate parameter '{}'",
                    span.line, span.column, name
                );
            }

            SemanticError::UndefinedVariable { name, span } => {
                eprintln!(
                    "Semantic error at {}:{}: undefined variable '{}'",
                    span.line, span.column, name
                );
            }

            SemanticError::UndefinedFunction { name, span } => {
                eprintln!(
                    "Semantic error at {}:{}: undefined function '{}'",
                    span.line, span.column, name
                );
            }

            SemanticError::MismatchedArgumentCount {
                called_function_name,
                provided_argument_count,
                expected_argument_count,
                span,
            } => {
                eprintln!(
                    "Semantic error at {}:{}: function '{}' called with {} argument(s), but {} expected",
                    span.line, span.column,
                    called_function_name,
                    provided_argument_count,
                    expected_argument_count
                );
            }

            SemanticError::BreakOutsideLoop { span } => {
                eprintln!(
                    "Semantic error at {}:{}: 'break' outside loop",
                    span.line, span.column
                );
            }

            SemanticError::ContinueOutsideLoop { span } => {
                eprintln!(
                    "Semantic error at {}:{}: 'continue' outside loop",
                    span.line, span.column
                );
            }

            SemanticError::InvalidTopLevelStatement { span } => {
                eprintln!(
                    "Semantic error at {}:{}: only functions are allowed at the top level",
                    span.line, span.column
                );
            },

            SemanticError::MismatchedReturnType { expected_return_type, provided_return_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, return value type of {:?} does not match the function's return type of {:?}",
                    span.line, span.column, provided_return_type, expected_return_type
                )
            },

            SemanticError::MismatchedBinaryOperationType { left_type, right_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, left operand type of {:?} does not match the right operand type of {:?}",
                    span.line, span.column, left_type, right_type
                )
            },

            SemanticError::MissingReturn { span } => {
                eprintln!(
                    "Semantic error at {}:{}, all functions must end with an explicit return",
                    span.line, span.column
                )
            },

            SemanticError::MismatchedVariableType { name, expected_type, provided_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, expected variable {} type of {:?} does not match the value's type of {:?}",
                    span.line, span.column, name, expected_type, provided_type
                )
            },

            SemanticError::InvalidType { var_name, var_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, {:?} variable of type {:?} is not allowed",
                    span.line, span.column, var_name, var_type
                )
            },

            SemanticError::InvalidEntryReturnType { span } => {
                eprintln!(
                    "Semantic error at {}:{}, entry function's return type must be int32",
                    span.line, span.column
                )
            },

            SemanticError::IntegerOverflow { span } => {
                eprintln!(
                    "Semantic error at {}:{}, an integer overflow occurred",
                    span.line, span.column
                )
            },

            SemanticError::MismatchedArgumentType { expected_type, provided_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, provided argument type of {:?} did not match expected type of {:?}",
                    span.line, span.column, provided_type, expected_type
                )
            },

            SemanticError::MismatchedAssignmentType { expected_type, provided_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, provided assignment type of {:?} did not match expected type of {:?}",
                    span.line, span.column, provided_type, expected_type
                )
            },

            SemanticError::InvalidTypeWidening { from_type, to_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, widening from {:?} to {:?} is not allowed",
                    span.line, span.column, from_type, to_type
                )
            },

            SemanticError::InvalidUnaryOperation { operand_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, a unary operation on operand of type: {:?} is not allowed",
                    span.line, span.column, operand_type
                )
            },

            SemanticError::UselessExpression { span } => {
                eprintln!(
                    "Semantic error at {}:{}, useless expression, only function calls can stand alone as a statement",
                    span.line, span.column
                )
            },

            SemanticError::InvalidConditionType { provided_type, span } => {
                eprintln!(
                    "Semantic error at {}:{}, condiitons can only be boolean, provided type is {:?}",
                    span.line, span.column, provided_type
                )
            }
        }
    }
}

fn llvm_optimize_and_link(ll_path: &str, output_exe: &str) {
    let opt_status = Command::new("opt")
        .args(["-O2", ll_path, "-o", "out.opt.bc"])
        .status()
        .expect("Failed to run opt");

    if !opt_status.success() {
        panic!("LLVM optimization failed");
    }

    let llc_status = Command::new("llc")
        .args(["out.opt.bc", "-filetype=obj", "-relocation-model=pic", "-o", "out.o"])
        .status()
        .expect("Failed to run llc");

    if !llc_status.success() {
        panic!("LLVM code generation failed");
    }

    let clang_status = Command::new("clang")
        .args(["out.o", "-o", output_exe])
        .status()
        .expect("Failed to run clang");

    if !clang_status.success() {
        panic!("Linking failed");
    }

    println!("Executable '{}' produced", output_exe);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Incorrect usage");
        return;
    }
    
    let input = read_file(&args[1]);

    let mut lexer = Lexer::from(input);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("Lexer error: {:?}", e);
            return;
        }
    };

    println!("Tokens:");
    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser::from(tokens);
    let mut program_tree = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            match e {
                ParserError::UnexpectedToken(token) => {
                    eprintln!(
                        "Parser error at line {}, column {}: unexpected token {:?}",
                        token.line, token.column, token.kind
                    );
                },

                ParserError::UnexpectedEndOfInput => {
                    eprintln!("Parser error: Unexpected end of input");
                },
                
                ParserError::UnexpectedType(_) => {
                    eprintln!("Parser error: Unexpected type");
                },
                
                ParserError::GenericError => {
                    eprintln!("Parser error: Generic error");
                },
                
                ParserError::EndOfInput => {
                    eprintln!("Parser error: End of input");
                },
            
                ParserError::UnexpectedBody => {
                    eprintln!("Parser error: Unexpected body");
                }
            }
            return;
        }
    };

    for toplevel in &program_tree {
        match toplevel {
            TopLevel::Function(f) => {
                println!("Function: {}, Return Type: {:?}, Parameters: {:?}", f.name, f.return_type, f.parameters);
                if let Some(f_body) = &f.body {
                    print_statement(f_body, 1);
                }
            }
            TopLevel::Statement(s) => print_statement(&s, 1),
        }
    }

    let mut semantic_analyzer = SemanticAnalyzer::from(program_tree);
    let program_tree = semantic_analyzer.analyze();
    
    // if diagnostics.has_errors() {
    //     print_semantic_errors(&diagnostics);
    //     return;
    // }

    println!("\nAST After semantic analysis:");
    for toplevel in &program_tree {
        match toplevel {
            TopLevel::Function(f) => {
                println!("Function: {}, Return Type: {:?}, Parameters: {:?}", f.name, f.return_type, f.parameters);
                if let Some(f_body) = &f.body {
                    print_statement(f_body, 1);
                }
            }
            TopLevel::Statement(s) => print_statement(&s, 1),
        }
    }

    print_semantic_errors(&semantic_analyzer.context.diagnostics);

    // let mut codegen_backend = LLVMCodeGenerator::default();
    // let output = match generate_program(program_tree, &mut codegen_backend) {
    //     Ok(out) => out,
    //     Err(e) => {
    //         eprintln!("Codegen error: {:?}", e);
    //         return;
    //     }
    // };

    // println!("{}", output);

    // write_file("out.ll".to_string(), &output);
    // llvm_optimize_and_link("out.ll", "out");

    // To be reactivated later as an alternative compilation path
    // write_file("out.asm".to_string(), &output);
    // assemble_and_link("out.asm", "out");
}