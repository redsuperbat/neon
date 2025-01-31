use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use neon_core::compiler::Compiler;
use rustyline::{error::ReadlineError, DefaultEditor, Editor};
use std::{
    fs,
    io::{self, Write},
};

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
