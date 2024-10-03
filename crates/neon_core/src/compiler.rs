use crate::{
    diagnostic::{DiagnosticsList, ToDiagnostic},
    interpreter::{EvaluationContext, Interpreter, Value},
    lexer::Lexer,
    lua_compiler::LuaCompiler,
    parser::{Expression, Parser},
    std::{fmt::FMT, io::IO},
    symbol_table::{Scope, SymbolTable},
    type_checker::{TypeChecker, TypeEnvironment},
};

pub struct Compiler {
    lua_compiler: LuaCompiler,
    interpreter: Interpreter,

    // Mutatates during parsing and evaluating
    type_env: TypeEnvironment,
    scope: Scope,
    eval_ctx: EvaluationContext,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
            lua_compiler: LuaCompiler::new(),

            scope: Scope::global(),
            type_env: TypeEnvironment::new(),
            eval_ctx: EvaluationContext::new(),
        }
    }

    fn parse_source(&self, src: &str) -> Result<Expression, DiagnosticsList> {
        let tokens = Lexer::new(src).collect();
        let mut diagnostics = DiagnosticsList::new();
        match Parser::new(tokens)
            .parse_program()
            .map_err(|e| e.to_diagnostic())
        {
            Ok(ast) => Ok(ast),
            Err(e) => {
                diagnostics.add(e);
                Err(diagnostics)
            }
        }
    }

    fn register_library(&mut self, lib: &str) {
        let ast = match self.parse_source(lib) {
            Ok(ast) => ast,
            Err(_dl) => return (),
        };
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl).visit_expression(&ast, &mut self.scope);
        let _ = self
            .interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx);
    }

    pub fn register_libraries(&mut self) {
        self.register_library(FMT);
        self.register_library(IO);
    }

    pub fn compile_lua(&self, src: &str) -> Result<String, DiagnosticsList> {
        let ast = self.parse_source(src)?;
        Ok(self.lua_compiler.compile_expression(&ast))
    }

    pub fn check(&mut self, src: &str) -> DiagnosticsList {
        let ast = match self.parse_source(src) {
            Ok(ast) => ast,
            Err(dl) => return dl,
        };
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl).visit_expression(&ast, &mut self.scope);
        dl
    }

    pub fn run(&mut self, src: &str) -> Result<Value, String> {
        let ast = self.parse_source(src).map_err(|l| l.to_string())?;
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl).visit_expression(&ast, &mut self.scope);

        if dl.has_errors() {
            return Err(dl.to_string());
        };
        self.interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx)
            .map_err(|e| e.kind.to_string())
    }
}
