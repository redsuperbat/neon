use clap::{Parser as ClapParser, Subcommand, ValueEnum};
use std::fs;

mod build;
mod exec;
mod lua;
mod repl;

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
        out: String,
    },
    /// Build the neon application
    Build {
        /// Path to neon file to build
        path: String,
        #[arg(short, long)]
        /// Set the output file path
        out: String,
    },
    /// Run the neon application
    Run {},
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum CompilationTarget {
    Lua,
}

fn main() {
    let args = Args::parse();

    match args.command {
        Some(command) => {
            match command {
                Commands::Compile { path, target, out } => {
                    let result = match target {
                        CompilationTarget::Lua => lua::compile(&path),
                    };
                    let Some(result) = result else {
                        return;
                    };
                    fs::write(out, result).expect("Should be able to write");
                }
                Commands::Build { path, out } => build::run(&path, &out),
                Commands::Run {} => todo!(),
            }
            // If we hit a command we should not do anything else
            return;
        }
        None => (),
    };

    match args.path {
        Some(path) => exec::file(&path),
        None => repl::start(),
    };
}
