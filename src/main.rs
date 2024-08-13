mod interpreter;
mod lexer;
mod parser;
mod symbol_table;

use interpreter::{EvaluationContext, Interpreter};
use lexer::Lexer;
use parser::{Expression, ExpressionKind, Parser, ParserError};
use std::{
    collections::HashMap,
    io::{self, BufRead, Write},
};
use symbol_table::{SymbolError, SymbolTable};

fn print(str: &str) -> () {
    print!("{str}");
    io::stdout().flush().expect("Failed to flush to stdout");
}

fn main() {
    let handle = io::stdin().lock();

    print("> ");

    for line in handle.lines() {
        let line = line.expect("Failed to read line") + "\n";

        let tokens = Lexer::new(line).vec();
        let interpreter = Interpreter::new();
        let ast = Parser::new(tokens).parse_program("main");

        let ast = match ast {
            Ok(ast) => ast,
            Err(e) => {
                match e {
                    ParserError::InvalidInteger { lexeme } => println!("invalid integer {lexeme}"),
                    ParserError::UnexpectedEndOfFile => println!("Unexpected end of program"),
                    ParserError::UnexpectedEndOfFunction => {
                        println!("Function did not end with expression")
                    }
                    ParserError::UnexpectedToken { expected, found } => {
                        let found = found.lexeme;
                        let expected = expected
                            .into_iter()
                            .map(|t| t.to_string())
                            .collect::<Vec<_>>()
                            .join(", ");
                        println!("Unexpected token '{found}' expected any of {:?}", expected)
                    }
                };
                print("> ");
                continue;
            }
        };

        println!("{:?}", ast);

        let mut symbol_table = SymbolTable::new();
        match symbol_table.visit_expression(&ast) {
            Ok(_) => (),
            Err(err) => {
                match err {
                    SymbolError::ExitGlobal => {
                        println!("Cannot move out of global scope")
                    }
                    SymbolError::UndefinedReference { name } => {
                        println!("Reference {name} is not defined")
                    }
                    SymbolError::ReDeclaration { name } => {
                        println!("Cannot redeclare symbol {name} in current scope")
                    }
                }
                print("> ");
                continue;
            }
        };

        let mut ctx = EvaluationContext {
            symbol_table,
            bindings: HashMap::new(),
            call_stack: vec![],
        };
        interpreter
            .evaluate_expression(&ast, &mut ctx)
            .expect("Should evaluate to a function expression properly");
        let result = interpreter.evaluate_expression(
            &Expression {
                start: (0, 0),
                end: (0, 0),
                kind: ExpressionKind::Invocation {
                    name: "main".to_string(),
                    arguments: vec![],
                },
            },
            &mut ctx,
        );

        println!("{:?}", result);

        print("> ");
    }
}
