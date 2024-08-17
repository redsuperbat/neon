use neon_core::{
    interpreter::{EvaluationContext, Interpreter},
    lexer::Lexer,
    parser::Parser,
    program::{execute_program, ProgramError},
    symbol_table::SymbolTable,
};
use std::{
    collections::HashMap,
    env, fs,
    io::{self, BufRead, Write},
};

fn print(str: &str) -> () {
    print!("{str}");
    io::stdout().flush().expect("Failed to flush to stdout");
}

fn repl() {
    let handle = io::stdin().lock();

    let mut ctx = EvaluationContext {
        symbol_table: SymbolTable::new(),
        call_stack: vec![],
        bindings: HashMap::new(),
    };
    let interpreter = Interpreter::new();

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line") + "\n";
        let tokens = Lexer::new(line).vec();

        let ast = match Parser::new(tokens).parse_program() {
            Ok(ast) => ast,
            Err(e) => {
                println!("{}", ProgramError::SyntaxError(e));
                print("> ");
                continue;
            }
        };

        match ctx.symbol_table.visit_expression(&ast) {
            Ok(_) => (),
            Err(e) => {
                println!("{}", ProgramError::SymbolError(e));
                print("> ");
                continue;
            }
        }

        ctx = EvaluationContext {
            symbol_table: ctx.symbol_table,
            bindings: ctx.bindings,
            call_stack: ctx.call_stack,
        };

        let result = match interpreter.evaluate_expression(&ast, &mut ctx) {
            Ok(ast) => ast,
            Err(e) => {
                println!("{}", ProgramError::RuntimeError(e));
                print("> ");
                continue;
            }
        };
        println!("{}", result);

        print("> ");
    }
}

fn file(path: &str) {
    let src = fs::read_to_string(path).expect("File not found");
    match execute_program(&src) {
        Ok(result) => println!("{:?}", result),
        Err(e) => println!("{}", e),
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
