use crate::parser::{Expression, ExpressionKind};
use std::{
    collections::{HashMap, HashSet},
    mem,
};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

    pub fn get_declaration(&self, reference: &str) -> Option<&String> {
        self.declarations_by_reference.get(reference)
    }

    pub fn visit_expression(&mut self, expression: &Expression) -> Result<(), SymbolError> {
        match &expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body,
            } => self.visit_fn(name, parameters, body),
            ExpressionKind::Identifier { name } => self.visit_identifier(name),
            ExpressionKind::Invocation { callee, arguments } => {
                self.visit_invocation(callee, arguments)
            }
            ExpressionKind::LetBinding { name, right } => self.visit_let(name, right),
            ExpressionKind::Block { body, return_val } => self.visit_block(body, return_val),
            ExpressionKind::If {
                predicate,
                if_block,
                else_block,
            } => self.visit_if(predicate, if_block, else_block),

            ExpressionKind::BinaryAdd { left, right } => self.visit_binary(left, right),
            ExpressionKind::BinaryNe { left, right } => self.visit_binary(left, right),
            ExpressionKind::BinaryEq { left, right } => self.visit_binary(left, right),
            ExpressionKind::BinarySubtract { left, right } => self.visit_binary(left, right),

            ExpressionKind::Int { .. } => Ok(()),
            ExpressionKind::Bool { .. } => Ok(()),
        }
    }

    fn visit_if(
        &mut self,
        predicate: &Expression,
        if_block: &Expression,
        else_block: &Option<Expression>,
    ) -> Result<(), SymbolError> {
        self.visit_expression(predicate)?;
        self.visit_expression(if_block)?;
        if let Some(else_block) = else_block {
            self.visit_expression(else_block)?;
        };
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
        block: &Expression,
    ) -> Result<(), SymbolError> {
        self.scope.declare(name)?;
        self.enter_scope(parameters);
        self.visit_expression(block)?;
        self.exit_scope()?;
        Ok(())
    }

    fn visit_block(
        &mut self,
        body: &Vec<Expression>,
        return_val: &Expression,
    ) -> Result<(), SymbolError> {
        for exp in body {
            self.visit_expression(exp)?;
        }
        self.visit_expression(return_val)
    }

    fn visit_invocation(
        &mut self,
        callee: &Expression,
        arguments: &Vec<Expression>,
    ) -> Result<(), SymbolError> {
        self.visit_expression(callee)?;
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

    fn visit_binary(&mut self, left: &Expression, right: &Expression) -> Result<(), SymbolError> {
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
        let ast = Parser::new(tokens).parse_expression().expect("Should work");
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
