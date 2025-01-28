use neon_core::compiler::Compiler;
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

fn repl() {
    let handle = io::stdin().lock();
    let mut compiler = Compiler::new();
    compiler.register_libraries();

    print("> ");
    for line in handle.lines() {
        let line = line.expect("Failed to read line");

        match compiler.run(&line) {
            Ok(v) => println!("{}", v),
            Err(e) => println!("{}", e),
        };

        print("> ");
    }
}

fn file(path: &str) {
    let src = fs::read_to_string(path).expect("File not found");
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    match compiler.run(&src) {
        Ok(v) => println!("{}", v),
        Err(e) => println!("{}", e),
    };
}

fn compile_to_lua(path: &str) -> Result<String, ()> {
    let src = fs::read_to_string(path).expect("File not found");
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    match compiler.compile_lua(&src) {
        Ok(v) => Ok(v),
        Err(dl) => {
            dl.diagnostics
                .iter()
                .for_each(|d| println!("{}", d.to_string()));
            Err(())
        }
    }
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
