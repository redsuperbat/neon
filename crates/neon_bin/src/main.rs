use neon_core::{
    interpreter::{Builtin, EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    lua_compiler::LuaCompiler,
    parser::{BuiltinExpressionKind, Parser},
    symbol_table::SymbolTable,
    type_checker::{TypeChecker, TypeEnvironment},
};
use std::{
    fs,
    io::{self, BufRead, Write},
};

use clap::{Parser as ClapParser, Subcommand, ValueEnum};

/// Neon CLI
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Filepath to neon script
    path: Option<String>,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compiles neon code to specified target
    Compile {
        /// Path to neon file to compile
        path: String,
        /// Set the compilation target
        #[arg(short, long, value_enum)]
        target: CompilationTarget,
        /// Set the output file path
        #[arg(short, long)]
        filepath: String,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum CompilationTarget {
    Lua,
}

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

fn program() -> (EvaluationContext, Interpreter, TypeEnvironment) {
    let mut ctx = EvaluationContext::new();
    let kind = &BuiltinExpressionKind::Print;
    ctx.register_bultin(kind);

    let mut interpreter = Interpreter::new();
    interpreter.register_bultin(kind, Box::new(Print {}));

    let mut env = TypeEnvironment::new();
    env.register_bultin(kind);

    (ctx, interpreter, env)
}

fn repl() {
    let handle = io::stdin().lock();
    let (mut ctx, interpreter, mut env) = program();

    let mut symbol_table = SymbolTable::new();

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line");

        let mut ts = TypeChecker::new();
        let tokens = Lexer::new(line).vec();

        let ast = match Parser::new(tokens).parse_program() {
            Ok(ast) => ast,
            Err(e) => {
                println!("{}", e.kind.to_string());
                print("> ");
                continue;
            }
        };

        ts.typeof_expression(&ast, &mut env);
        symbol_table.visit_expression(&ast);

        let dl = symbol_table
            .diagnostics_list
            .merge(&mut ts.diagnostics_list);

        symbol_table.diagnostics_list.clear();

        if dl.has_errors() {
            dl.diagnostics.iter().for_each(|d| println!("{}", d));
            print("> ");
            continue;
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

    let (mut ctx, interpreter, mut env) = program();
    let mut ts = TypeChecker::new();
    let mut symbol_table = SymbolTable::new();

    ts.typeof_expression(&ast, &mut env);
    symbol_table.visit_expression(&ast);

    let dl = ts
        .diagnostics_list
        .merge(&mut symbol_table.diagnostics_list);
    if dl.has_errors() {
        dl.diagnostics.iter().for_each(|d| println!("{}", d));
        print("> ");
        return;
    }

    match interpreter.evaluate_expression(&ast, &mut ctx) {
        Ok(result) => println!("{:?}", result),
        Err(e) => println!("{}", e.kind.to_string()),
    }
}

fn compile_to_lua(path: &str) -> Result<String, ()> {
    let src = fs::read_to_string(path).expect("File not found");
    let tokens = Lexer::new(src).vec();

    let ast = match Parser::new(tokens).parse_program() {
        Ok(v) => v,
        Err(e) => {
            println!("{}", e.kind.to_string());
            return Err(());
        }
    };

    let mut env = TypeEnvironment::new();
    env.register_bultin(&BuiltinExpressionKind::Print);
    let mut ts = TypeChecker::new();
    let mut symbol_table = SymbolTable::new();
    symbol_table.register_bultin(&BuiltinExpressionKind::Print);

    ts.typeof_expression(&ast, &mut env);
    symbol_table.visit_expression(&ast);

    let dl = ts
        .diagnostics_list
        .merge(&mut symbol_table.diagnostics_list);
    if dl.has_errors() {
        dl.diagnostics.iter().for_each(|d| println!("{}", d));
        return Err(());
    }
    let le = LuaCompiler::new();

    Ok(le.compile_expression(&ast))
}

fn main() {
    let args = Args::parse();

    match args.command {
        Some(command) => {
            match command {
                Commands::Compile {
                    path,
                    target,
                    filepath,
                } => {
                    let result = match target {
                        CompilationTarget::Lua => compile_to_lua(&path),
                    };
                    let Ok(result) = result else {
                        return;
                    };
                    fs::write(filepath, result).expect("Should be able to write");
                }
            }
            // If we hit a command we should not do anything else
            return;
        }
        None => (),
    };

    match args.path {
        Some(src_filepath) => file(&src_filepath),
        None => repl(),
    };
}
