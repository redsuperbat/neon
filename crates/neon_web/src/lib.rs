use neon_core::program::execute_program;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn interpret_src(src: &str) -> Result<String, String> {
    match execute_program(src) {
        Ok(result) => Ok(result.to_string()),
        Err(err) => Ok(err.to_string()),
    }
}
