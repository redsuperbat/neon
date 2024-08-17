use std::{collections::HashMap, fmt::Display};

use crate::{
    interpreter::{EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    parser::{Parser, ParserError},
    symbol_table::{SymbolError, SymbolTable},
};

pub enum ProgramError {
    ParserError(ParserError),
    SymbolError(SymbolError),
    RuntimeError(RuntimeError),
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::ParserError(e) => match e {
                ParserError::InvalidInteger { lexeme } => write!(f, "invalid integer {lexeme}"),
                ParserError::UnexpectedEndOfFile => write!(f, "Unexpected end of program"),
                ParserError::UnexpectedEndOfBlock => {
                    write!(f, "Block did not end with expression")
                }
                ParserError::UnexpectedToken { expected, found } => {
                    let expected = expected
                        .into_iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(
                        f,
                        "Unexpected token '{}' {:?}:{:?} expected any of {:?}",
                        found.lexeme, found.start, found.end, expected
                    )
                }
                _ => write!(f, "{:?}", e),
            },
            ProgramError::RuntimeError(e) => match e {
                RuntimeError::TypeError => write!(f, "Invalid type"),
                RuntimeError::UndefinedReference => write!(f, "Undefined reference"),
                RuntimeError::UninitializedVariable => write!(f, "Uninitialized variable"),
                RuntimeError::IllegalInvocation => write!(f, "Illegal invocation"),
                _ => write!(f, "{:?}", e),
            },
            ProgramError::SymbolError(e) => match e {
                SymbolError::ExitGlobal => {
                    write!(f, "Cannot move out of global scope")
                }
                SymbolError::UndefinedReference { name } => {
                    write!(f, "Reference {name} is not defined")
                }
                SymbolError::ReDeclaration { name } => {
                    write!(f, "Cannot redeclare symbol {name} in current scope")
                }
                _ => write!(f, "{:?}", e),
            },
        }
    }
}

pub fn execute_program(source: &str) -> Result<Value, ProgramError> {
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
