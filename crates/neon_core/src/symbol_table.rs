use crate::{
    location::Location,
    parser::{
        ArrayNode, BinaryOperationNode, BlockNode, BuiltinExpressionKind, Expression, FnNode,
        ForLoopNode, ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode, InvocationNode,
        LetBinding, ObjectNode,
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
        match &expression {
            Expression::Fn(e) => self.visit_fn(e),
            Expression::Identifier(e) => self.visit_identifier(e),
            Expression::Invocation(e) => self.visit_invocation(e),
            Expression::LetBinding(e) => self.visit_let(e),
            Expression::Block(e) => self.visit_block(e),
            Expression::If(e) => self.visit_if(e),
            Expression::Else(e) => self.visit_expression(&e.consequent),
            Expression::Binary(e) => self.visit_binary(e),
            Expression::Object(e) => self.visit_object(e),
            Expression::Array(e) => self.visit_array(e),
            Expression::IndexAccess(IndexAccessNode { indexee, index, .. }) => {
                self.visit_expression(indexee)?;
                self.visit_expression(index)
            }
            Expression::ForLoop(e) => self.visit_for_loop(e),

            Expression::PropertyAccess(e) => self.visit_expression(&e.object),

            Expression::Int(..) => Ok(()),
            Expression::Bool(..) => Ok(()),
            Expression::String(..) => Ok(()),
            Expression::Builtin(..) => Ok(()),
            Expression::Empty(..) => Ok(()),
        }
    }

    fn visit_object(&mut self, object: &ObjectNode) -> Result<(), SymbolError> {
        for property in &object.properties {
            self.visit_expression(property.value.as_ref())?;
        }
        Ok(())
    }

    fn visit_for_loop(&mut self, for_loop: &ForLoopNode) -> Result<(), SymbolError> {
        let ForLoopNode {
            iterable,
            body,
            targets,
            ..
        } = for_loop;

        self.visit_expression(iterable)?;

        match targets {
            ForLoopTarget::Single(target) => {
                self.enter_scope(&vec![target.name.to_string()]);
            }
            ForLoopTarget::Tuple(first, second) => {
                self.enter_scope(&vec![first.to_string(), second.to_string()]);
            }
        };

        self.visit_expression(body)?;

        self.exit_scope()?;

        Ok(())
    }

    fn visit_array(&mut self, array: &ArrayNode) -> Result<(), SymbolError> {
        for el in &array.elements {
            self.visit_expression(el)?;
        }
        Ok(())
    }

    fn visit_if(&mut self, exp: &IfNode) -> Result<(), SymbolError> {
        let IfNode {
            predicate,
            consequent,
            alternate,
            ..
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

    fn visit_fn(&mut self, exp: &FnNode) -> Result<(), SymbolError> {
        let FnNode {
            name,
            parameters,
            body,
            ..
        } = exp;
        self.scope.declare(name);
        self.enter_scope(parameters);
        self.visit_expression(body)?;
        self.exit_scope()?;
        Ok(())
    }

    fn visit_block(&mut self, block: &BlockNode) -> Result<(), SymbolError> {
        let BlockNode {
            body, return_val, ..
        } = block;
        for exp in body {
            self.visit_expression(exp)?;
        }
        self.visit_expression(return_val)
    }

    fn visit_invocation(&mut self, exp: &InvocationNode) -> Result<(), SymbolError> {
        let InvocationNode {
            callee, arguments, ..
        } = exp;
        self.visit_expression(callee)?;
        for arg in arguments {
            self.visit_expression(arg)?;
        }
        Ok(())
    }

    fn visit_identifier(&mut self, identifier: &IdentifierNode) -> Result<(), SymbolError> {
        self.scope
            .has_identifier(&identifier.name)
            .map(|_| ())
            .ok_or(SymbolError {
                kind: SymbolErrorKind::UndefinedReference(identifier.name.to_string()),
                loc: identifier.loc,
            })
    }

    fn visit_let(&mut self, l: &LetBinding) -> Result<(), SymbolError> {
        let LetBinding { right, name, .. } = l;
        self.visit_expression(right)?;
        self.scope.declare(name);
        Ok(())
    }

    fn visit_binary(&mut self, bin: &BinaryOperationNode) -> Result<(), SymbolError> {
        let BinaryOperationNode { left, right, .. } = bin;
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
