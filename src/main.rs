mod token;
mod lexer;
mod parser;
mod backend;
mod backends;
mod semantic_analyzer;

use std::{env, fs, process::Command};

use crate::semantic_analyzer::{ SemanticAnalyzer, SemanticError, Diagnostics };
use crate::token::print_token;
use crate::lexer::{ Lexer, LexerError };
use crate::parser::{ Parser, TopLevel, Statement, Expression, ParserError };
use crate::backend::generate_program;
// use crate::backends::asm_codegen::ASMCodeGenerator;
use crate::backends::llvm_codegen::LLVMCodeGenerator;

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
            print_expression(value, indent + 1);
        }

        Statement::VariableDeclare {
            var_type,
            name,
            initializer,
            span
        } => {
            println!("{}Declare {:?} {}", padding, var_type, name);
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

    match expr {
        Expression::Variable { name, span } => {
            println!("{}Variable {}", padding, name);
        }

        Expression::IntLiteral64 { value, span } => {
            println!("{}Int {}", padding, value);
        },

        Expression::UnaryOperation { operator, operand, span } => {
            println!("{}Unary {:?}", padding, operator);
            print_expression(operand, indent + 1);
        }

        Expression::BinaryOperation {
            left,
            operator,
            right,
            span
        } => {
            println!("{}Binary {:?}", padding, operator);
            print_expression(left, indent + 1);
            print_expression(right, indent + 1);
        }

        Expression::FunctionCall { called: callee, arguments, span } => {
            println!("{}Call:", padding);
            print_expression(callee, indent + 1);
            for arg in arguments {
                print_expression(arg, indent + 1);
            }
        },

        Expression::Null { span } => {
            println!("{}Null", padding);
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

            SemanticError::MissingReturnType { expected, span } => {
                eprintln!(
                    "Semantic error at {}:{}, expected a return type of {:?}",
                    span.line, span.column, *expected
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
        }
    }
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
                }
                ParserError::UnexpectedEndOfInput => {
                    eprintln!("Parser error: unexpected end of input");
                }
                ParserError::UnexpectedType(_) => {
                    eprintln!("Parser error: unexpected type");
                }
                ParserError::GenericError => {
                    eprintln!("Parser error: generic error");
                }
                ParserError::EndOfInput => {
                    eprintln!("Parser error: end of input");
                }
            }
            return;
        }
    };

    for toplevel in &program_tree {
        match toplevel {
            TopLevel::Function(f) => {
                println!("Function: {}, Return Type: {:?}", f.name, f.return_type);
                print_statement(&f.body, 1);
            }
            TopLevel::Statement(s) => print_statement(&s, 1),
        }
    }

    let mut semantic_analyzer = SemanticAnalyzer::from(&mut program_tree);
    let diagnostics = semantic_analyzer.analyze();

    if diagnostics.has_errors() {
        print_semantic_errors(&diagnostics);
        return;
    }

    let mut codegen_backend = LLVMCodeGenerator::default();
    let output = match generate_program(program_tree, &mut codegen_backend) {
        Ok(out) => out,
        Err(e) => {
            eprintln!("Codegen error: {:?}", e);
            return;
        }
    };

    println!("{}", output);

    write_file("out.asm".to_string(), &output);
    assemble_and_link("out.asm", "out");
}
