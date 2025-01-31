use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use neon_core::compiler::Compiler;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::fs;

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

fn repl() {
    let mut rl = DefaultEditor::new().expect("Could not get cmd editor");
    let mut compiler = Compiler::new();
    compiler.register_libraries();

    let mut ctrl_c = false;
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                match compiler.run(&line) {
                    Ok(v) => println!("{}", v),
                    Err(e) => println!("{}", e),
                };
                ctrl_c = false;
            }
            Err(ReadlineError::Interrupted) => {
                if ctrl_c {
                    break;
                } else {
                    println!("(To exit, press Ctrl+C again or Ctrl+D)");
                    ctrl_c = true;
                }
            }
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
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

fn compile_to_lua(path: &str) -> Option<String> {
    let src = fs::read_to_string(path).expect(&format!("File {path} not found"));
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    match compiler.compile_lua(&src) {
        Ok(v) => Some(v),
        Err(dl) => {
            println!("{}", dl.to_string());
            None
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
                    let Some(result) = result else {
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
