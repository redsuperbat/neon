use std::{fs, process::Command};

use neon_core::compiler::Compiler;
use rs_transpiler::RustTranspiler;
use tempfile::TempDir;

mod rs_transpiler;

pub fn run(path: &str, out: &str) {
    let src = fs::read_to_string(path).expect("File not found");
    let mut compiler = Compiler::new();
    let ast = match compiler.check_and_parse(&src) {
        Ok(r) => r,
        Err(dl) => {
            println!("{}", dl.to_string());
            return;
        }
    };
    let mut type_env = compiler.cloned_type_env();
    let rust_ir = RustTranspiler::new(&mut type_env).transpile_program(&ast);
    let dir = TempDir::new().expect("Should be able to create tempdir");
    let file_path = dir.path().with_file_name("tmp.rs");
    fs::write(&file_path, &rust_ir).expect("Should be able to write file to tmp dir");

    let output = Command::new("rustc")
        .arg(&file_path)
        .arg(&format!("-o ./{}", out))
        .output()
        .expect("Failed to execute command");

    if output.status.success() {
        println!("Output:\n{}", String::from_utf8_lossy(&output.stdout));
    } else {
        eprintln!("Error:\n{}", String::from_utf8_lossy(&output.stderr));
    }
}
