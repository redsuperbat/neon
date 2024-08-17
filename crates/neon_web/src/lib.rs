use neon_core::program::{execute_program, ProgramError};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct ProgramOk {
    result: String,
}

#[wasm_bindgen(getter_with_clone)]
struct ProgramErr {
    line: usize,
    col: usize,
    message: String,
}

#[wasm_bindgen]
pub fn interpret_src(src: &str) -> Result<ProgramOk, ProgramErr> {
    match execute_program(src) {
        Ok(result) => Ok(ProgramOk {
            result: result.to_string(),
        }),
        Err(err) => {
            let err = match err {
                ProgramError::SyntaxError(e) => ProgramErr {
                    col: e.start.0,
                    line: e.start.1,
                    message: e.kind.to_string(),
                },
                ProgramError::SymbolError(e) => ProgramErr {
                    message: e.kind.to_string(),
                    line: e.start.0,
                    col: e.start.1,
                },
                ProgramError::RuntimeError(e) => ProgramErr {
                    col: 0,
                    line: 0,
                    message: e.kind.to_string(),
                },
            };
            Err(err)
        }
    }
}
