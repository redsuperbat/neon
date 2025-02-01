use neon_core::compiler::Compiler;
use std::fs;

pub fn file(path: &str) {
    let src = fs::read_to_string(path).expect("File not found");
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    match compiler.run(&src) {
        Ok(v) => println!("{}", v),
        Err(e) => println!("{}", e),
    };
}
