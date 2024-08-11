use crate::parser::{Expression, ExpressionKind};
use std::{
    collections::{HashMap, HashSet},
    mem,
};

#[derive(Debug)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    declarations: HashSet<String>,
}

impl Scope {
    fn global() -> Scope {
        Scope {
            declarations: HashSet::new(),
            parent: None,
        }
    }

    pub fn declare(&mut self, id: &str) -> Result<(), SymbolError> {
        if self.has_identifier(id).is_some() {
            return Err(SymbolError::ReDeclaration {
                name: id.to_string(),
            });
        }

        self.declarations.insert(id.to_string());
        Ok(())
    }

    pub fn has_identifier(&self, id: &str) -> Option<String> {
        if self.declarations.contains(id) {
            return Some(id.to_string());
        }

        if let Some(parent) = &self.parent {
            return parent.has_identifier(id);
        }

        return None;
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scope: Scope,
    declarations_by_reference: HashMap<String, String>,
}

#[derive(Debug)]
pub enum SymbolError {
    ExitGlobal,
    UndefinedReference { name: String },
    ReDeclaration { name: String },
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scope: Scope::global(),
            declarations_by_reference: HashMap::new(),
        }
    }

    pub fn visit_expression(&mut self, expression: &Expression) -> Result<(), SymbolError> {
        match &expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body,
            } => self.visit_fn(name, parameters, body),
            ExpressionKind::Identifier { name } => self.visit_identifier(name),
            ExpressionKind::Invocation { name, arguments } => {
                self.visit_invocation(name, arguments)
            }
            ExpressionKind::LetBinding { name, right } => self.visit_let(name, right),
            ExpressionKind::Int { value } => self.visit_int(value),
            ExpressionKind::BinaryAdd { left, right } => self.visit_binary_add(left, right),
            ExpressionKind::Program { body } => self.visit_program(body),
        }
    }

    fn visit_program(&mut self, body: &Vec<Expression>) -> Result<(), SymbolError> {
        for exp in body {
            self.visit_expression(exp)?;
        }
        Ok(())
    }

    fn enter_scope(&mut self, identifiers: &Vec<String>) {
        let mut set = HashSet::new();
        for id in identifiers {
            set.insert(id.to_string());
        }
        let parent = mem::replace(
            &mut self.scope,
            Scope {
                parent: None,
                declarations: set,
            },
        );

        self.scope.parent = Some(Box::new(parent));
    }

    fn exit_scope(&mut self) -> Result<(), SymbolError> {
        let current = mem::replace(&mut self.scope.parent, None);
        match current {
            Some(current) => {
                self.scope = *current;
                Ok(())
            }
            None => Err(SymbolError::ExitGlobal),
        }
    }

    fn visit_fn(
        &mut self,
        name: &str,
        parameters: &Vec<String>,
        body: &Vec<Expression>,
    ) -> Result<(), SymbolError> {
        self.scope.declare(name)?;
        self.enter_scope(parameters);
        for exp in body {
            self.visit_expression(exp)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_invocation(
        &mut self,
        name: &str,
        arguments: &Vec<Expression>,
    ) -> Result<(), SymbolError> {
        self.visit_identifier(name)?;
        for arg in arguments {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_identifier(&mut self, name: &str) -> Result<(), SymbolError> {
        if let Some(reference) = self.scope.has_identifier(name) {
            self.declarations_by_reference
                .insert(name.to_string(), reference.to_string());
            Ok(())
        } else {
            Err(SymbolError::UndefinedReference {
                name: name.to_string(),
            })
        }
    }

    fn visit_let(&mut self, name: &str, right: &Expression) -> Result<(), SymbolError> {
        self.visit_expression(right)?;
        self.scope.declare(name)?;
        Ok(())
    }

    fn visit_int(&mut self, _value: &i64) -> Result<(), SymbolError> {
        Ok(())
    }

    fn visit_binary_add(
        &mut self,
        left: &Expression,
        right: &Expression,
    ) -> Result<(), SymbolError> {
        self.visit_expression(left)?;
        self.visit_expression(right)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn visit_ok() {
        let code = String::from("let a = 3; fn t(){ 3 + 4 }; t();");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();

        let res = st.visit_expression(&ast);
        assert!(res.is_ok());
    }

    #[test]
    fn visit_fail() {
        let code = String::from("fn t() {}; f();");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();

        let res = st.visit_expression(&ast);

        assert!(res.is_err());
    }
}
