mod lexer;
mod parser;
use std::io::{self, BufRead, Write};

use lexer::Lexer;
use parser::Parser;

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
        println!(
            "{:?}",
            tokens
                .clone()
                .into_iter()
                .map(|t| t.kind)
                .collect::<Vec<_>>()
        );
        let ast = Parser::new(tokens).parse_expression();

        println!("{:?}", ast);
        print("> ");
    }
}
