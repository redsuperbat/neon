use crate::{
    diagnostic::{DiagnosticsList, ToDiagnostic},
    interpreter::{EvaluationContext, Interpreter, Value},
    lexer::Lexer,
    parser::{Expression, Parser},
    std::io::IO,
    symbol_table::{Scope, SymbolTable},
    types::{type_checker::TypeChecker, type_env::TypeEnvironment},
    visitor::Visitor,
};

pub struct Compiler {
    interpreter: Interpreter,

    // Mutates during parsing and evaluating
    type_env: TypeEnvironment,
    scope: Scope,
    eval_ctx: EvaluationContext,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),

            scope: Scope::global(),
            type_env: TypeEnvironment::new(),
            eval_ctx: EvaluationContext::new(),
        }
    }

    pub fn cloned_type_env(&self) -> TypeEnvironment {
        self.type_env.clone_with_bindings()
    }

    pub fn parse_source(&self, src: &str) -> Result<Expression, DiagnosticsList> {
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

    pub fn check_and_parse(&mut self, src: &str) -> Result<Expression, DiagnosticsList> {
        let ast = self.parse_source(src)?;
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl, &mut self.scope)
            .scanner()
            .scan_expression(&ast);
        if dl.has_errors() {
            return Err(dl);
        }
        Ok(ast)
    }

    fn register_library(&mut self, lib: &str) {
        let ast = match self.parse_source(lib) {
            Ok(ast) => ast,
            Err(_dl) => return (),
        };
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl, &mut self.scope)
            .scanner()
            .scan_expression(&ast);
        let _ = self
            .interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx);
    }

    pub fn register_libraries(&mut self) {
        self.register_library(IO);
    }

    pub fn check(&mut self, src: &str) -> DiagnosticsList {
        let ast = match self.parse_source(src) {
            Ok(ast) => ast,
            Err(dl) => return dl,
        };
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl, &mut self.scope)
            .scanner()
            .scan_expression(&ast);
        dl
    }

    pub fn run(&mut self, src: &str) -> Result<Value, String> {
        let ast = self.parse_source(src).map_err(|l| l.to_string())?;
        let mut dl = DiagnosticsList::new();
        TypeChecker::new(&mut dl).typeof_expression(&ast, &mut self.type_env);
        SymbolTable::new(&mut dl, &mut self.scope)
            .scanner()
            .scan_expression(&ast);

        if dl.has_errors() {
            return Err(dl.to_string());
        };
        self.interpreter
            .evaluate_expression(&ast, &mut self.eval_ctx)
            .map_err(|e| e.kind.to_string())
    }
}
