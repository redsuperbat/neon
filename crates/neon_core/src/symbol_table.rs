use crate::{
    diagnostic::{Diagnostic, DiagnosticsList, ErrorDiagnostic, UndefinedReferenceError},
    parser::{
        ArrayNode, AssignmentNode, BinaryOperationNode, BlockNode, Expression, FnNode, ForLoopNode,
        ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode, InvocationNode, LetBindingNode,
        ObjectNode,
    },
};
use std::{collections::HashSet, mem};

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    declarations: HashSet<String>,
}

impl Scope {
    pub fn global() -> Scope {
        Scope {
            declarations: HashSet::new(),
            parent: None,
        }
    }

    pub fn declare(&mut self, id: &str) {
        self.declarations.insert(id.to_string());
    }

    pub fn has_identifier(&self, identifier: &IdentifierNode) -> bool {
        let id = &identifier.name;
        if self.declarations.contains(id) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.has_identifier(identifier);
        }

        false
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    scope: Scope,
}

impl SymbolTable {
    pub fn new(scope: Scope) -> SymbolTable {
        SymbolTable { scope }
    }

    pub fn visit_expression(&mut self, expression: &Expression, dl: &mut DiagnosticsList) {
        match &expression {
            Expression::Fn(node) => self.visit_fn(node, dl),
            Expression::Identifier(node) => self.visit_identifier(node, dl),
            Expression::Invocation(node) => self.visit_invocation(node, dl),
            Expression::LetBinding(node) => self.visit_let(node, dl),
            Expression::Block(node) => self.visit_block(node, dl),
            Expression::If(node) => self.visit_if(node, dl),
            Expression::Binary(node) => self.visit_binary(node, dl),
            Expression::Array(node) => self.visit_array(node, dl),
            Expression::ForLoop(node) => self.visit_for_loop(node, dl),
            Expression::Object(node) => self.visit_object(node, dl),
            Expression::Else(node) => self.visit_expression(&node.consequent, dl),
            Expression::IndexAccess(node) => self.visit_index_access(&node, dl),
            Expression::PropertyAccess(node) => self.visit_expression(&node.object, dl),
            Expression::Assignment(node) => self.visit_assignment(node, dl),

            Expression::Use(..) => (),
            Expression::Int(..) => (),
            Expression::Bool(..) => (),
            Expression::String(..) => (),
            Expression::Builtin(..) => (),
            Expression::Empty(..) => (),
        };
    }

    fn visit_index_access(&mut self, node: &IndexAccessNode, dl: &mut DiagnosticsList) {
        self.visit_expression(&node.indexee, dl);
        self.visit_expression(&node.index, dl);
    }

    fn visit_assignment(&mut self, node: &AssignmentNode, dl: &mut DiagnosticsList) {
        self.visit_identifier(&node.identifier, dl);
        self.visit_expression(&node.right, dl);
    }

    fn visit_object(&mut self, object: &ObjectNode, dl: &mut DiagnosticsList) {
        self.enter_scope(&vec![]);
        for property in &object.properties {
            self.visit_expression(property.value.as_ref(), dl);
        }
        self.exit_scope();
    }

    fn visit_for_loop(&mut self, for_loop: &ForLoopNode, dl: &mut DiagnosticsList) {
        let ForLoopNode {
            iterable,
            body,
            targets,
            ..
        } = for_loop;

        self.visit_expression(iterable, dl);

        match targets {
            ForLoopTarget::Single(target) => {
                self.enter_scope(&vec![target.clone()]);
            }
            ForLoopTarget::Tuple(first, second) => {
                self.enter_scope(&vec![first.clone(), second.clone()]);
            }
        };

        self.visit_expression(body, dl);

        self.exit_scope()
    }

    fn visit_array(&mut self, array: &ArrayNode, dl: &mut DiagnosticsList) {
        for el in &array.elements {
            self.visit_expression(el, dl);
        }
    }

    fn visit_if(&mut self, exp: &IfNode, dl: &mut DiagnosticsList) {
        let IfNode {
            predicate,
            consequent,
            alternate,
            ..
        } = exp;

        self.visit_expression(predicate, dl);
        self.visit_expression(consequent, dl);

        if let Some(alternate) = alternate.as_ref() {
            self.visit_expression(alternate, dl);
        }
    }

    fn enter_scope(&mut self, identifiers: &Vec<IdentifierNode>) {
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

    fn exit_scope(&mut self) {
        let current = mem::replace(&mut self.scope.parent, None);
        match current {
            Some(current) => {
                self.scope = *current;
            }
            None => panic!("Internal neon error"),
        }
    }

    fn visit_fn(&mut self, exp: &FnNode, dl: &mut DiagnosticsList) {
        let FnNode {
            identifier,
            parameters,
            body,
            ..
        } = exp;
        self.scope.declare(&identifier.name);
        let identifiers = parameters.iter().map(|t| t.identifier.clone()).collect();
        self.enter_scope(&identifiers);
        self.visit_expression(body, dl);
        self.exit_scope();
    }

    fn visit_block(&mut self, block: &BlockNode, dl: &mut DiagnosticsList) {
        let BlockNode {
            body, return_val, ..
        } = block;
        for exp in body {
            self.visit_expression(exp, dl);
        }
        self.visit_expression(return_val, dl);
    }

    fn visit_invocation(&mut self, exp: &InvocationNode, dl: &mut DiagnosticsList) {
        let InvocationNode {
            callee, arguments, ..
        } = exp;
        self.visit_expression(callee, dl);
        for arg in arguments {
            self.visit_expression(arg, dl);
        }
    }

    fn visit_identifier(&mut self, identifier: &IdentifierNode, dl: &mut DiagnosticsList) {
        if self.scope.has_identifier(&identifier) {
            return;
        }
        dl.add(Diagnostic::Error(ErrorDiagnostic::UndefinedReference(
            UndefinedReferenceError {
                loc: identifier.loc,
                name: identifier.name.to_string(),
            },
        )));
    }

    fn visit_let(&mut self, l: &LetBindingNode, dl: &mut DiagnosticsList) {
        let LetBindingNode { right, binding, .. } = l;
        if let Some(right) = right.as_ref() {
            self.visit_expression(right, dl);
        };
        self.scope.declare(&binding.identifier.name);
    }

    fn visit_binary(&mut self, bin: &BinaryOperationNode, dl: &mut DiagnosticsList) {
        let BinaryOperationNode { left, right, .. } = bin;
        self.visit_expression(left, dl);
        self.visit_expression(right, dl)
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
        let scope = Scope::global();
        let mut st = SymbolTable::new(scope);
        let mut dl = DiagnosticsList::new();
        st.visit_expression(&ast, &mut dl);
        assert!(!dl.has_errors())
    }

    #[test]
    fn visit_fail() {
        let code = String::from("fn f(){5} f()");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let scope = Scope::global();
        let mut st = SymbolTable::new(scope);
        let mut dl = DiagnosticsList::new();

        st.visit_expression(&ast, &mut dl);

        assert!(!dl.has_errors())
    }
}
