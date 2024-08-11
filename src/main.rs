mod lexer;
mod parser;
mod symbol_table;

use lexer::Lexer;
use parser::{Parser, ParserError};
use std::io::{self, BufRead, Write};
use symbol_table::SymbolTable;

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
        let ast = Parser::new(tokens).parse_expression();

        let ast = match ast {
            Ok(ast) => ast,
            Err(e) => {
                match e {
                    ParserError::InvalidInteger { lexeme } => println!("invalid integer {lexeme}"),
                    ParserError::EmptyExpression { start, end } => {
                        println!("Empty expressions are not allowed {:?}:{:?}", start, end)
                    }
                    ParserError::UnexpectedEOF => println!("Unexpected end of file in expression"),
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
        match symbol_table.visit_expression(ast) {
            Ok(_) => (),
            Err(err) => match err {
                symbol_table::ReferenceError::ExitGlobalError => {
                    println!("Cannot move out of global scope")
                }
                symbol_table::ReferenceError::UndefinedReference { name } => {
                    println!("Reference {name} is not defined")
                }
            },
        };

        print("> ");
    }
}
