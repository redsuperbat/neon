mod semantic_analyzer;

use js_sys::Array;
use neon_core::{
    compiler::Compiler,
    diagnostic::DiagnosticsList,
    interpreter::{ForeignFunctionInterface, RuntimeError, Value},
    lexer::Lexer,
    location::{Location, Pos},
};
use semantic_analyzer::{SemanticAnalyzer, SemanticToken};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct JsPos {
    pub line: usize,
    pub col: usize,
}

impl From<Pos> for JsPos {
    fn from(value: Pos) -> Self {
        JsPos {
            col: value.column(),
            line: value.line(),
        }
    }
}

#[wasm_bindgen(getter_with_clone)]
pub struct JsToken {
    pub kind: String,
    pub start: JsPos,
    pub end: JsPos,
}

impl From<SemanticToken> for JsToken {
    fn from(value: SemanticToken) -> Self {
        JsToken {
            start: JsPos::from(value.start),
            end: JsPos::from(value.end),
            kind: value.kind.to_string(),
        }
    }
}

#[wasm_bindgen]
pub fn tokenize(src: &str) -> Array {
    let tokens = Lexer::new(src).collect::<Vec<_>>();
    SemanticAnalyzer::new(tokens)
        .map(JsToken::from)
        .map(JsValue::from)
        .collect()
}

#[wasm_bindgen]
pub fn print_ast(src: &str) -> Result<String, CompilationDiagnostics> {
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    match compiler.check_and_parse(src) {
        Ok(ast) => Ok(format!("{:#?}", ast)),
        Err(dl) => Err(dl.into()),
    }
}

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<(), CompilationDiagnostics> {
    let mut compiler = Compiler::new();
    compiler.register_libraries();
    let dl = compiler.check(src);
    if dl.has_errors() {
        return Err(dl.into());
    }
    Ok(())
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = window)]
    fn on_print(s: &str);
}

#[wasm_bindgen(getter_with_clone)]
pub struct ProgramOk {
    pub result: String,
}

impl From<Location> for JsLocation {
    fn from(value: Location) -> Self {
        JsLocation {
            start: value.start.into(),
            end: value.end.into(),
        }
    }
}

#[derive(Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct JsLocation {
    pub start: JsPos,
    pub end: JsPos,
}

#[derive(Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct CompilationDiagnostic {
    pub loc: JsLocation,
    pub message: String,
}

#[wasm_bindgen(getter_with_clone)]
pub struct CompilationDiagnostics {
    pub errors: Vec<CompilationDiagnostic>,
}

impl From<DiagnosticsList> for CompilationDiagnostics {
    fn from(dl: DiagnosticsList) -> Self {
        CompilationDiagnostics {
            errors: dl
                .diagnostics
                .iter()
                .map(|d| CompilationDiagnostic {
                    message: d.to_string(),
                    loc: d.loc.into(),
                })
                .collect::<Vec<_>>(),
        }
    }
}

struct Print {}
impl ForeignFunctionInterface for Print {
    fn exec(&self, values: Vec<Value>) -> Result<Value, RuntimeError> {
        let str = values
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        on_print(&str);
        Ok(Value::Unit)
    }
}

#[wasm_bindgen]
pub fn interpret_src(src: &str) -> Result<ProgramOk, CompilationDiagnostics> {
    let mut compiler = Compiler::new();
    compiler.register_libraries();

    compiler
        .run(src)
        .map(|r| ProgramOk {
            result: r.to_string(),
        })
        .map_err(|e| CompilationDiagnostics {
            errors: vec![CompilationDiagnostic {
                message: e,
                loc: Location::beginning().into(),
            }],
        })
}
