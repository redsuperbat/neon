mod interpreter;
mod lexer;
mod parser;
mod symbol_table;

use interpreter::{EvaluationContext, Interpreter, RuntimeError, Value};
use lexer::Lexer;
use parser::{Parser, ParserError};
use std::{
    collections::HashMap,
    env, fs,
    io::{self, BufRead, Write},
};
use symbol_table::{SymbolError, SymbolTable};

fn print(str: &str) -> () {
    print!("{str}");
    io::stdout().flush().expect("Failed to flush to stdout");
}

enum ProgramError {
    ParserError(ParserError),
    SymbolError(SymbolError),
    RuntimeError(RuntimeError),
}

impl ProgramError {
    fn print(&self) {
        match self {
            ProgramError::ParserError(e) => match e {
                ParserError::InvalidInteger { lexeme } => println!("invalid integer {lexeme}"),
                ParserError::UnexpectedEndOfFile => println!("Unexpected end of program"),
                ParserError::UnexpectedEndOfBlock => {
                    println!("Block did not end with expression")
                }
                ParserError::UnexpectedToken { expected, found } => {
                    let expected = expected
                        .into_iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    println!(
                        "Unexpected token '{}' {:?}:{:?} expected any of {:?}",
                        found.lexeme, found.start, found.end, expected
                    )
                }
                _ => println!("{:?}", e),
            },
            ProgramError::RuntimeError(e) => match e {
                RuntimeError::TypeError => println!("Invalid type"),
                RuntimeError::UndefinedReference => println!("Undefined reference"),
                RuntimeError::UninitializedVariable => println!("Uninitialized variable"),
                RuntimeError::IllegalInvocation => println!("Illegal invocation"),
                _ => println!("{:?}", e),
            },
            ProgramError::SymbolError(e) => match e {
                SymbolError::ExitGlobal => {
                    println!("Cannot move out of global scope")
                }
                SymbolError::UndefinedReference { name } => {
                    println!("Reference {name} is not defined")
                }
                SymbolError::ReDeclaration { name } => {
                    println!("Cannot redeclare symbol {name} in current scope")
                }
                _ => println!("{:?}", e),
            },
        }
    }
}

fn execute_program(source: &str) -> Result<Value, ProgramError> {
    let tokens = Lexer::new(source).vec();
    let interpreter = Interpreter::new();
    let ast = Parser::new(tokens)
        .parse_program()
        .map_err(|e| ProgramError::ParserError(e))?;

    let mut symbol_table = SymbolTable::new();

    symbol_table
        .visit_expression(&ast)
        .map_err(|e| ProgramError::SymbolError(e))?;

    let mut ctx = EvaluationContext {
        symbol_table,
        bindings: HashMap::new(),
        call_stack: vec![],
    };

    interpreter
        .evaluate_expression(&ast, &mut ctx)
        .map_err(|e| ProgramError::RuntimeError(e))
}

fn repl() {
    let handle = io::stdin().lock();

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line") + "\n";

        match execute_program(&line) {
            Ok(result) => println!("{:?}", result),
            Err(e) => e.print(),
        }

        print("> ");
    }
}

fn file(path: &str) {
    let src = fs::read_to_string(path).expect("File not found");
    println!("{src}");
    match execute_program(&src) {
        Ok(result) => println!("{:?}", result),
        Err(e) => e.print(),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let input_args = &args[1..];

    if let Some(file_path) = input_args.first() {
        file(file_path);
    } else {
        repl();
    }
}
