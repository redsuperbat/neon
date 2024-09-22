mod semantic_analyzer;

use js_sys::Array;
use neon_core::{
    diagnostic::{Diagnostic, DiagnosticsList},
    interpreter::{Builtin, EvaluationContext, Interpreter, RuntimeError, Value},
    lexer::Lexer,
    location::{Location, Pos},
    parser::{BuiltinExpressionKind, Parser},
    symbol_table::SymbolTable,
    type_checker::{TypeChecker, TypeEnvironment},
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
pub fn compile(src: &str) -> Result<(), ProgramTypeErr> {
    let tokens = Lexer::new(src).collect::<Vec<_>>();
    let ast = Parser::new(tokens)
        .parse_program()
        .map_err(|e| ProgramTypeErr {
            errors: vec![TypeError {
                loc: Location::new(e.start, e.end).into(),
                message: e.kind.to_string(),
            }],
        })?;
    let mut symbol_table = SymbolTable::new();
    let mut dl = DiagnosticsList::new();
    let mut ts = TypeChecker::new(&mut dl);
    let mut env = TypeEnvironment::new(symbol_table);
    ts.typeof_expression(&ast, &mut env);
    if dl.has_errors() {
        return Err(ProgramTypeErr {
            errors: dl
                .diagnostics
                .iter()
                .map(|d| match d {
                    Diagnostic::Error(e) => e,
                })
                .map(|d| TypeError {
                    message: d.message(),
                    loc: d.loc().into(),
                })
                .collect::<Vec<_>>(),
        });
    } else {
        Ok(())
    }
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

#[wasm_bindgen(getter_with_clone)]
pub struct ProgramErr {
    pub start: JsPos,
    pub end: JsPos,
    pub message: String,
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

#[wasm_bindgen(getter_with_clone)]
pub struct TypeError {
    pub loc: JsLocation,
    pub message: String,
}

#[wasm_bindgen(getter_with_clone)]
pub struct ProgramTypeErr {
    errors: Vec<TypeError>,
}

struct Print {}
impl Builtin for Print {
    fn exec(&self, values: Vec<&Value>) -> Result<Value, RuntimeError> {
        let str = values
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        on_print(&str);
        Ok(Value::Unit)
    }
}

fn program() -> (EvaluationContext, Interpreter) {
    let mut symbol_table = SymbolTable::new();
    symbol_table.register_bultin(&BuiltinExpressionKind::Print);

    let mut ctx = EvaluationContext::new(symbol_table);
    ctx.register_bultin(&BuiltinExpressionKind::Print);

    let mut interpreter = Interpreter::new();
    interpreter.register_bultin(&BuiltinExpressionKind::Print, Box::new(Print {}));
    (ctx, interpreter)
}

#[wasm_bindgen]
pub fn interpret_src(src: &str) -> Result<ProgramOk, ProgramErr> {
    let tokens = Lexer::new(src).collect::<Vec<_>>();
    let ast = Parser::new(tokens)
        .parse_program()
        .map_err(|e| ProgramErr {
            start: JsPos::from(e.start),
            end: JsPos::from(e.end),
            message: e.kind.to_string(),
        })?;

    let (mut ctx, interpreter) = program();

    ctx.symbol_table
        .visit_expression(&ast)
        .map_err(|e| ProgramErr {
            start: JsPos::from(e.loc.start),
            end: JsPos::from(e.loc.end),
            message: e.kind.to_string(),
        })?;

    interpreter
        .evaluate_expression(&ast, &mut ctx)
        .map(|r| ProgramOk {
            result: r.to_string(),
        })
        .map_err(|e| ProgramErr {
            start: JsPos::from(e.loc.start),
            end: JsPos::from(e.loc.end),
            message: e.kind.to_string(),
        })
}
