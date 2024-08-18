use std::usize;

use js_sys::Array;
use neon_core::{
    lexer::{Lexer, Pos, Token, TokenKind},
    parser::Parser,
    program::{execute_program, ProgramError},
    symbol_table::SymbolTable,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct JsPos(pub usize, pub usize);

impl From<Pos> for JsPos {
    fn from(value: Pos) -> Self {
        JsPos(value.0, value.1)
    }
}

#[wasm_bindgen(getter_with_clone)]
pub struct JsToken {
    pub kind: String,
    pub start: JsPos,
    pub end: JsPos,
}

fn to_class_name(kind: &TokenKind) -> String {
    let str = match kind {
        TokenKind::FnKeyword => "fn",
        TokenKind::LetKeyword => "let",
        TokenKind::IfKeyword => "if",
        TokenKind::ElseKeyword => "else",
        TokenKind::TrueKeyword => "true",
        TokenKind::FalseKeyword => "false",

        TokenKind::OpenAngleBracket => "open-angle",
        TokenKind::ClosedAngleBracket => "cloded-angle",
        TokenKind::MinusOperator => "minus",
        TokenKind::PlusOperator => "plus",
        TokenKind::Ampersand => "ampersand",
        TokenKind::Pipe => "pipe",

        TokenKind::SemiColon => "semi",
        TokenKind::Percentage => "percentage",
        TokenKind::Equals => "equals",
        TokenKind::Bang => "bang",
        TokenKind::OpenCurlyBrace => "curly",
        TokenKind::ClosedCurlyBrace => "curly",
        TokenKind::OpenParen => "paren",
        TokenKind::ClosedParen => "paren",
        TokenKind::Comma => "comma",
        TokenKind::Newline => "newline",
        TokenKind::IntegerLiteral => "integer",
        TokenKind::StringLiteral => "string",
        TokenKind::Symbol => "symbol",
        TokenKind::WhiteSpace => "whitespace",
        TokenKind::Unknown => "unknown",

        _ => "unknown",
    };
    str.to_string()
}

impl From<Token> for JsToken {
    fn from(value: Token) -> Self {
        JsToken {
            start: JsPos::from(value.start),
            end: JsPos::from(value.end),
            kind: to_class_name(&value.kind),
        }
    }
}

#[wasm_bindgen]
pub fn tokenize(src: &str) -> Array {
    Lexer::new(src)
        .map(JsToken::from)
        .map(JsValue::from)
        .collect()
}

#[wasm_bindgen]
pub fn compile(src: &str) -> Result<(), ProgramErr> {
    let tokens = Lexer::new(src).collect::<Vec<_>>();
    let ast = Parser::new(tokens)
        .parse_program()
        .map_err(|e| ProgramErr {
            start: JsPos::from(e.start),
            end: JsPos::from(e.end),
            message: e.kind.to_string(),
        })?;
    let mut symbol_table = SymbolTable::new();
    symbol_table
        .visit_expression(&ast)
        .map_err(|e| ProgramErr {
            start: JsPos::from(e.start),
            end: JsPos::from(e.end),
            message: e.kind.to_string(),
        })?;
    Ok(())
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

#[wasm_bindgen]
pub fn interpret_src(src: &str) -> Result<ProgramOk, ProgramErr> {
    match execute_program(src) {
        Ok(result) => Ok(ProgramOk {
            result: result.to_string(),
        }),
        Err(err) => {
            let err = match err {
                ProgramError::SyntaxError(e) => ProgramErr {
                    start: JsPos::from(e.start),
                    end: JsPos::from(e.end),
                    message: e.kind.to_string(),
                },
                ProgramError::SymbolError(e) => ProgramErr {
                    message: e.kind.to_string(),
                    start: JsPos::from(e.start),
                    end: JsPos::from(e.end),
                },
                ProgramError::RuntimeError(e) => ProgramErr {
                    start: JsPos::from(e.start),
                    end: JsPos::from(e.end),
                    message: e.kind.to_string(),
                },
            };
            Err(err)
        }
    }
}
