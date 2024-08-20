use crate::{
    lexer::Pos,
    parser::{BuiltinExpressionKind, Expression, ExpressionKind},
};
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

    pub fn declare(&mut self, id: &str, start: &Pos, end: &Pos) -> Result<(), SymbolError> {
        if self.has_identifier(id).is_some() {
            return Err(SymbolError {
                start: start.clone(),
                end: end.clone(),
                kind: SymbolErrorKind::ReDeclaration {
                    name: id.to_string(),
                },
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
pub struct SymbolError {
    pub kind: SymbolErrorKind,
    pub start: Pos,
    pub end: Pos,
}

impl ToString for SymbolErrorKind {
    fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug)]
pub enum SymbolErrorKind {
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

    pub fn register_bultin(&mut self, kind: BuiltinExpressionKind) {
        let name = kind.name();
        self.declarations_by_reference
            .insert(name.clone(), name.clone());
        self.scope.declarations.insert(name);
    }

    pub fn visit_expression(&mut self, expression: &Expression) -> Result<(), SymbolError> {
        let Expression { start, end, .. } = &expression;
        match &expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body,
            } => self.visit_fn(name, parameters, body, start, end),
            ExpressionKind::Identifier { name } => self.visit_identifier(name, start, end),
            ExpressionKind::Invocation { callee, arguments } => {
                self.visit_invocation(callee, arguments)
            }
            ExpressionKind::LetBinding { name, right } => self.visit_let(name, right, start, end),
            ExpressionKind::Block { body, return_val } => self.visit_block(body, return_val),
            ExpressionKind::If {
                predicate,
                consequent,
                alternate,
            } => self.visit_if(predicate, consequent, alternate),
            ExpressionKind::Else { consequent } => self.visit_expression(consequent),

            ExpressionKind::Int { .. } => Ok(()),
            ExpressionKind::Bool { .. } => Ok(()),
            ExpressionKind::String { .. } => Ok(()),
            ExpressionKind::Empty => Ok(()),

            ExpressionKind::Add { left, right } => self.visit_binary(left, right),
            ExpressionKind::Sub { left, right } => self.visit_binary(left, right),
            ExpressionKind::Ne { left, right } => self.visit_binary(left, right),
            ExpressionKind::Eq { left, right } => self.visit_binary(left, right),
            ExpressionKind::Lt { left, right } => self.visit_binary(left, right),
            ExpressionKind::Gt { left, right } => self.visit_binary(left, right),
            ExpressionKind::Modulus { left, right } => self.visit_binary(left, right),
            ExpressionKind::And { left, right } => self.visit_binary(left, right),
            ExpressionKind::Or { left, right } => self.visit_binary(left, right),
            ExpressionKind::Builtin { .. } => Ok(()),
        }
    }

    fn visit_if(
        &mut self,
        predicate: &Expression,
        consequent: &Expression,
        else_block: &Option<Expression>,
    ) -> Result<(), SymbolError> {
        self.visit_expression(predicate)?;
        self.visit_expression(consequent)?;
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
            None => panic!("Internal neon error"),
        }
    }

    fn visit_fn(
        &mut self,
        name: &str,
        parameters: &Vec<String>,
        block: &Expression,
        start: &Pos,
        end: &Pos,
    ) -> Result<(), SymbolError> {
        self.scope.declare(name, start, end)?;
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

    fn visit_identifier(&mut self, name: &str, start: &Pos, end: &Pos) -> Result<(), SymbolError> {
        if let Some(reference) = self.scope.has_identifier(name) {
            self.declarations_by_reference
                .insert(name.to_string(), reference.to_string());
            Ok(())
        } else {
            Err(SymbolError {
                kind: SymbolErrorKind::UndefinedReference {
                    name: name.to_string(),
                },
                start: start.clone(),
                end: end.clone(),
            })
        }
    }

    fn visit_let(
        &mut self,
        name: &str,
        right: &Expression,
        start: &Pos,
        end: &Pos,
    ) -> Result<(), SymbolError> {
        self.visit_expression(right)?;
        self.scope.declare(name, start, end)?;
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
        let code = String::from("fn t() {5}; f()");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();

        let res = st.visit_expression(&ast);

        assert!(res.is_err());
    }
}
