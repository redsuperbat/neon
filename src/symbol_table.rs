use crate::parser::{Expression, ExpressionKind};
use std::{collections::HashSet, mem};

pub struct Scope {
    parent: Option<Box<Scope>>,
    identifiers: HashSet<String>,
}

impl Scope {
    fn global() -> Scope {
        Scope {
            identifiers: HashSet::new(),
            parent: None,
        }
    }

    fn has_identifier(&self, id: &str) -> bool {
        if self.identifiers.contains(id) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.has_identifier(id);
        }

        return false;
    }
}

pub struct SymbolTable {
    scope: Scope,
}

pub enum ReferenceError {
    ExitGlobalError,
    UndefinedReference { name: String },
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scope: Scope::global(),
        }
    }

    pub fn visit_expression(&mut self, expression: Expression) -> Result<(), ReferenceError> {
        match expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body,
            } => self.visit_fn(name, parameters, body),
            ExpressionKind::Identifier { name } => self.visit_identifier(name),
            ExpressionKind::Invocation { name, arguments } => {
                self.visit_invocation(name, arguments)
            }
            ExpressionKind::LetBinding { name } => self.visit_let(name),
            ExpressionKind::Int { value } => self.visit_int(value),
            ExpressionKind::BinaryAdd { left, right } => self.visit_binary_add(left, right),
            ExpressionKind::Assignment { left, right } => self.visit_assignment(left, right),
        }
    }

    fn enter_scope(&mut self, identifiers: Vec<String>) {
        let mut set = HashSet::new();
        for id in identifiers {
            set.insert(id);
        }
        let parent = mem::replace(
            &mut self.scope,
            Scope {
                parent: None,
                identifiers: set,
            },
        );

        self.scope.parent = Some(Box::new(parent));
    }

    fn exit_scope(&mut self) -> Result<(), ReferenceError> {
        let current = mem::replace(&mut self.scope.parent, None);
        match current {
            Some(current) => {
                self.scope = *current;
                Ok(())
            }
            None => Err(ReferenceError::ExitGlobalError),
        }
    }

    fn visit_fn(
        &mut self,
        name: String,
        mut parameters: Vec<String>,
        body: Vec<Expression>,
    ) -> Result<(), ReferenceError> {
        // This makes the fn recursive... I think?
        parameters.push(name);
        self.enter_scope(parameters);
        for exp in body {
            self.visit_expression(exp)?;
        }
        self.exit_scope()?;
        Ok(())
    }

    fn visit_invocation(
        &mut self,
        name: String,
        arguments: Vec<Expression>,
    ) -> Result<(), ReferenceError> {
        self.visit_identifier(name)?;
        for arg in arguments {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_identifier(&mut self, name: String) -> Result<(), ReferenceError> {
        if self.scope.has_identifier(&name) {
            Ok(())
        } else {
            Err(ReferenceError::UndefinedReference { name })
        }
    }

    fn visit_let(&mut self, name: String) -> Result<(), ReferenceError> {
        self.scope.identifiers.insert(name);
        Ok(())
    }

    fn visit_int(&mut self, _value: i64) -> Result<(), ReferenceError> {
        Ok(())
    }

    fn visit_binary_add(
        &mut self,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Result<(), ReferenceError> {
        self.visit_expression(*left)?;
        self.visit_expression(*right)?;
        Ok(())
    }

    fn visit_assignment(
        &mut self,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Result<(), ReferenceError> {
        self.visit_expression(*left)?;
        self.visit_expression(*right)?;
        Ok(())
    }
}
