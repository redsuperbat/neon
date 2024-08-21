use neon_core::{
    interpreter::{Builtin, EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    parser::{BuiltinExpressionKind, Parser},
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

fn program() -> (EvaluationContext, Interpreter) {
    let mut symbol_table = SymbolTable::new();
    symbol_table.register_bultin(&BuiltinExpressionKind::Print);

    let mut ctx = EvaluationContext::new(symbol_table);
    ctx.register_bultin(&BuiltinExpressionKind::Print);

    let mut interpreter = Interpreter::new();
    interpreter.register_bultin(&BuiltinExpressionKind::Print, Box::new(Print {}));
    (ctx, interpreter)
}

fn repl() {
    let handle = io::stdin().lock();
    let (mut ctx, interpreter) = program();

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line");
        let tokens = Lexer::new(line).vec();

        let ast = match Parser::new(tokens).parse_program() {
            Ok(ast) => ast,
            Err(e) => {
                println!("{}", e.kind.to_string());
                print("> ");
                continue;
            }
        };

        match ctx.symbol_table.visit_expression(&ast) {
            Ok(_) => (),
            Err(e) => {
                println!("{}", e.kind.to_string());
                print("> ");
                continue;
            }
        }

        let result = match interpreter.evaluate_expression(&ast, &mut ctx) {
            Ok(ast) => ast,
            Err(e) => {
                println!("{}", e.kind.to_string());
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
    let tokens = Lexer::new(src).vec();

    let ast = match Parser::new(tokens).parse_program() {
        Ok(v) => v,
        Err(e) => {
            println!("{}", e.kind.to_string());
            return;
        }
    };

    let (mut ctx, interpreter) = program();

    match interpreter.evaluate_expression(&ast, &mut ctx) {
        Ok(result) => println!("{:?}", result),
        Err(e) => println!("{}", e.kind.to_string()),
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
