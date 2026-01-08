mod token;
mod lexer;
mod parser;
mod backend;
mod backends;

use std::{env, fs, process::Command};

use crate::token::print_token;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::parser::TopLevel;
use crate::parser::Statement;
use crate::parser::Expression;
use crate::backend::generate_program;
use crate::backends::LLVMCodeGenerator;

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
        Statement::Return { value } => {
            println!("{}Return:", padding);
            print_expression(value, indent + 1);
        }

        Statement::VariableDeclare {
            var_type,
            name,
            initializer,
        } => {
            println!("{}Declare {:?} {}", padding, var_type, name);
            print_expression(initializer, indent + 1);
        }

        Statement::VariableAssignment { name, value } => {
            println!("{}Assign {}", padding, name);
            print_expression(value, indent + 1);
        }

        Statement::Block { statements } => {
            println!("{}Block:", padding);
            for stmt in statements {
                print_statement(stmt, indent + 1);
            }
        }

        Statement::Expression { expression } => {
            println!("{}Expression:", padding);
            print_expression(expression, indent + 1);
        }

        Statement::If {
            condition,
            then_branch,
            else_branch,
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

        Statement::While { condition, body } => {
            println!("{}While:", padding);
            print_expression(condition, indent + 1);
            print_statement(body, indent + 1);
        }

        Statement::Break => {
            println!("{}Break", padding);
        }

        Statement::Continue => {
            println!("{}Continue", padding);
        }
    }
}


fn print_expression(expr: &Expression, indent: usize) {
    let padding = "  ".repeat(indent);

    match expr {
        Expression::Variable { name } => {
            println!("{}Variable {}", padding, name);
        }

        Expression::IntLiteral { value } => {
            println!("{}Int {}", padding, value);
        }

        Expression::UnaryOperation { operator, operand } => {
            println!("{}Unary {:?}", padding, operator);
            print_expression(operand, indent + 1);
        }

        Expression::BinaryOperation {
            left,
            operator,
            right,
        } => {
            println!("{}Binary {:?}", padding, operator);
            print_expression(left, indent + 1);
            print_expression(right, indent + 1);
        }

        Expression::FunctionCall { callee, arguments } => {
            println!("{}Call:", padding);
            print_expression(callee, indent + 1);
            for arg in arguments {
                print_expression(arg, indent + 1);
            }
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

    let mut lexer = Lexer {
        input,
        index: 0,
    };

    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("lexer error: {:?}", e);
            return;
        }
    };

    println!("Tokens:");
    for token in &tokens {
        print_token(token);
    }

    let mut parser = Parser {
        tokens,
        index: 0,
    };

    let program = match parser.parse_program() {
        Ok(program) => program,
        Err(e) => {
            eprintln!("parser error: {:?}", e);
            return;
        }
    };

    for toplevel in &program {
        match toplevel {
            TopLevel::Function(f) => {
                println!("Function: {}", f.name);
                print_statement(&f.body, 1);
            }
            TopLevel::Statement(s) => print_statement(&s, 1),
        }
    }

    let mut llvm_codegen = LLVMCodeGenerator::default();

    let output = match generate_program(program, &mut llvm_codegen) {
        Ok(output) => output,
        Err(e) => {
            eprintln!("codegen error: {:?}", e);
            return;
        }
    };

    println!("{}", output);

    write_file(String::from("out.asm"), &output);
    assemble_and_link("out.asm", "out");
}