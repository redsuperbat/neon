use std::{collections::HashMap, fmt::Display};

use crate::{
    interpreter::{EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    parser::{Parser, SyntaxError, SyntaxErrorKind},
    symbol_table::{SymbolError, SymbolTable},
};

pub enum ProgramError {
    SyntaxError(SyntaxError),
    SymbolError(SymbolError),
    RuntimeError(RuntimeError),
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::SyntaxError(e) => match &e.kind {
                SyntaxErrorKind::UnexpectedEndOfFile => write!(f, "Unexpected end of program"),
                SyntaxErrorKind::UnexpectedEndOfBlock => {
                    write!(f, "Block did not end with expression")
                }
                SyntaxErrorKind::UnexpectedToken { expected, found } => {
                    let expected = expected
                        .into_iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(
                        f,
                        "Unexpected token '{}'  expected any of {:?}",
                        found.to_string(),
                        expected.to_string()
                    )
                }
                _ => write!(f, "{:?}", e),
            },
            ProgramError::RuntimeError(e) => match e {
                RuntimeError::TypeError => write!(f, "Invalid type"),
                RuntimeError::UndefinedReference => write!(f, "Undefined reference"),
                RuntimeError::UninitializedVariable => write!(f, "Uninitialized variable"),
                RuntimeError::IllegalInvocation => write!(f, "Illegal invocation"),
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
            },
        }
    }
}

pub fn execute_program(source: &str) -> Result<Value, ProgramError> {
    let tokens = Lexer::new(source).vec();
    let interpreter = Interpreter::new();
    let ast = Parser::new(tokens)
        .parse_program()
        .map_err(|e| ProgramError::SyntaxError(e))?;

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
