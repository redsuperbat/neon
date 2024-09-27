use crate::{
    diagnostic::{DiagnosticsList, ToDiagnostic},
    interpreter::{EvaluationContext, Interpreter, Value},
    lexer::Lexer,
    lua_compiler::LuaCompiler,
    parser::{Expression, Parser},
    std::fmt::FMT,
    symbol_table::{Scope, SymbolTable},
    type_checker::{TypeChecker, TypeEnvironment},
};
pub struct Compiler {
    type_checker: TypeChecker,
    lua_compiler: LuaCompiler,
    interpreter: Interpreter,
    symbol_table: SymbolTable,
    type_env: TypeEnvironment,
    eval_ctx: EvaluationContext,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            type_checker: TypeChecker::new(),
            interpreter: Interpreter::new(),
            symbol_table: SymbolTable::new(Scope::global()),
            type_env: TypeEnvironment::new(),
            eval_ctx: EvaluationContext::new(),
            lua_compiler: LuaCompiler::new(),
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

    pub fn register_libraries(&mut self) {
        let ast = match self.parse_source(FMT) {
            Ok(ast) => ast,
            Err(_dl) => return (),
        };
        let mut dl = DiagnosticsList::new();
        self.symbol_table.visit_expression(&ast, &mut dl);
        self.type_checker
            .typeof_expression(&ast, &mut self.type_env, &mut dl);
        let _ = self
            .interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx);
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
        self.symbol_table.visit_expression(&ast, &mut dl);
        self.type_checker
            .typeof_expression(&ast, &mut self.type_env, &mut dl);
        let _ = self
            .interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx);
        dl
    }

    pub fn run(&mut self, src: &str) -> Result<Value, String> {
        let ast = self.parse_source(src).map_err(|l| l.to_string())?;
        let mut dl = DiagnosticsList::new();
        self.symbol_table.visit_expression(&ast, &mut dl);
        self.type_checker
            .typeof_expression(&ast, &mut self.type_env, &mut dl);

        if dl.has_errors() {
            return Err(dl.to_string());
        };
        self.interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx)
            .map_err(|e| e.kind.to_string())
    }
}
