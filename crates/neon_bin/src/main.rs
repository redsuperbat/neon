use neon_core::{
    interpreter::{Builtin, EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    parser::{BuiltinExpressionKind, Parser},
    program::{execute_program, ProgramError},
    symbol_table::SymbolTable,
};
use std::{
    env, fs,
    io::{self, BufRead, Write},
};

fn print(str: &str) -> () {
    print!("{str}");
    io::stdout().flush().expect("Failed to flush to stdout");
}

struct Print {}
impl Builtin for Print {
    fn exec(&self, values: Vec<&Value>) -> Result<Value, RuntimeError> {
        for value in values {
            print!("{} ", value);
        }
        print!("\n");
        Ok(Value::Unit)
    }
}

fn repl() {
    let handle = io::stdin().lock();

    let mut symbol_table = SymbolTable::new();
    symbol_table.register_bultin(BuiltinExpressionKind::Print);

    let mut ctx = EvaluationContext::new(symbol_table);
    ctx.register_bultin(BuiltinExpressionKind::Print);

    let mut interpreter = Interpreter::new();
    interpreter.register_bultin(BuiltinExpressionKind::Print, Box::new(Print {}));

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line");
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
