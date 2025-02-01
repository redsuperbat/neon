use lua_transpiler::LuaTranspiler;
use neon_core::compiler::Compiler;
use std::fs;

mod lua_transpiler;

pub fn compile(path: &str) -> Option<String> {
    let src = fs::read_to_string(path).expect(&format!("File {path} not found"));
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    let ast = match compiler.parse_source(&src) {
        Ok(v) => Some(v),
        Err(dl) => {
            println!("{}", dl.to_string());
            None
        }
    }?;
    let lua_src = LuaTranspiler::new().compile_expression(&ast);
    Some(lua_src)
}
