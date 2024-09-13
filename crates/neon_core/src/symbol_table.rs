use crate::{
    location::Location,
    parser::{
        Binary, Block, BuiltinExpressionKind, Expression, ExpressionKind, Fn, ForLoop, If,
        IndexAccess, Invocation, LetBinding,
    },
};
use std::{collections::HashSet, mem};

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

    pub fn declare(&mut self, id: &str) {
        self.declarations.insert(id.to_string());
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
}

#[derive(Debug)]
pub struct SymbolError {
    pub kind: SymbolErrorKind,
    pub loc: Location,
}

impl ToString for SymbolErrorKind {
    fn to_string(&self) -> String {
        match self {
            SymbolErrorKind::UndefinedReference(name) => {
                format!("Undefined reference '{name}'")
            }
        }
    }
}

#[derive(Debug)]
pub enum SymbolErrorKind {
    UndefinedReference(String),
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scope: Scope::global(),
        }
    }

    pub fn register_bultin(&mut self, kind: &BuiltinExpressionKind) {
        self.scope.declarations.insert(kind.name());
    }

    pub fn visit_expression(&mut self, expression: &Expression) -> Result<(), SymbolError> {
        let Expression { loc, .. } = &expression;
        match &expression.kind {
            ExpressionKind::Fn(f) => self.visit_fn(f),
            ExpressionKind::Identifier(name) => self.visit_identifier(name, loc),
            ExpressionKind::Invocation(i) => self.visit_invocation(i),
            ExpressionKind::LetBinding(l) => self.visit_let(l),
            ExpressionKind::Block(b) => self.visit_block(b),
            ExpressionKind::If(exp) => self.visit_if(exp),
            ExpressionKind::Else(consequent) => self.visit_expression(consequent),
            ExpressionKind::Binary(bin) => self.visit_binary(bin),
            ExpressionKind::Builtin { .. } => Ok(()),
            ExpressionKind::Array(elements) => self.visit_array(elements),
            ExpressionKind::IndexAccess(IndexAccess { indexee, index }) => {
                self.visit_expression(indexee)?;
                self.visit_expression(index)
            }
            ExpressionKind::ForLoop(for_loop) => self.visit_for_loop(for_loop),
            ExpressionKind::PropertyAccess { .. } => todo!(),

            ExpressionKind::Int(..) => Ok(()),
            ExpressionKind::Bool(..) => Ok(()),
            ExpressionKind::String(..) => Ok(()),
            ExpressionKind::Empty => Ok(()),
        }
    }

    fn visit_for_loop(&mut self, for_loop: &ForLoop) -> Result<(), SymbolError> {
        let ForLoop {
            iterable,
            body,
            target,
        } = for_loop;

        self.visit_expression(iterable)?;

        self.enter_scope(&vec![]);
        self.visit_expression(target)?;
        self.visit_expression(body)?;

        self.exit_scope()?;

        Ok(())
    }

    fn visit_array(&mut self, elements: &Vec<Expression>) -> Result<(), SymbolError> {
        for el in elements {
            self.visit_expression(el)?;
        }
        Ok(())
    }

    fn visit_if(&mut self, exp: &If) -> Result<(), SymbolError> {
        let If {
            predicate,
            consequent,
            alternate,
        } = exp;

        self.visit_expression(predicate)?;
        self.visit_expression(consequent)?;

        if let Some(alternate) = alternate.as_ref() {
            self.visit_expression(alternate)?;
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
            None => panic!("Internal neon error"),
        }
    }

    fn visit_fn(&mut self, exp: &Fn) -> Result<(), SymbolError> {
        let Fn {
            name,
            parameters,
            body,
        } = exp;
        self.scope.declare(name);
        self.enter_scope(parameters);
        self.visit_expression(body)?;
        self.exit_scope()?;
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<(), SymbolError> {
        let Block { body, return_val } = block;
        for exp in body {
            self.visit_expression(exp)?;
        }
        self.visit_expression(return_val)
    }

    fn visit_invocation(&mut self, exp: &Invocation) -> Result<(), SymbolError> {
        let Invocation { callee, arguments } = exp;
        self.visit_expression(callee)?;
        for arg in arguments {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_identifier(&mut self, name: &str, loc: &Location) -> Result<(), SymbolError> {
        self.scope
            .has_identifier(name)
            .map(|_| ())
            .ok_or(SymbolError {
                kind: SymbolErrorKind::UndefinedReference(name.to_string()),
                loc: *loc,
            })
    }

    fn visit_let(&mut self, l: &LetBinding) -> Result<(), SymbolError> {
        let LetBinding { right, name } = l;
        self.visit_expression(right)?;
        self.scope.declare(name);
        Ok(())
    }

    fn visit_binary(&mut self, bin: &Binary) -> Result<(), SymbolError> {
        let Binary { left, right, .. } = bin;
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
