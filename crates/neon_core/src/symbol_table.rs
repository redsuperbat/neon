use crate::{
    diagnostic::{Diagnostic, DiagnosticsList, ErrorDiagnostic, UndefinedReferenceError},
    parser::{
        ArrayNode, AssignmentNode, BinaryOperationNode, BlockNode, BuiltinExpressionKind,
        Expression, FnNode, ForLoopNode, ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode,
        InvocationNode, LetBindingNode, ObjectNode,
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
    pub diagnostics_list: DiagnosticsList,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            scope: Scope::global(),
            diagnostics_list: DiagnosticsList::new(),
        }
    }

    pub fn register_bultin(&mut self, kind: &BuiltinExpressionKind) {
        self.scope.declarations.insert(kind.name());
    }

    pub fn visit_expression(&mut self, expression: &Expression) {
        match &expression {
            Expression::Fn(node) => self.visit_fn(node),
            Expression::Identifier(node) => self.visit_identifier(node),
            Expression::Invocation(node) => self.visit_invocation(node),
            Expression::LetBinding(node) => self.visit_let(node),
            Expression::Block(node) => self.visit_block(node),
            Expression::If(node) => self.visit_if(node),
            Expression::Else(node) => self.visit_expression(&node.consequent),
            Expression::Binary(node) => self.visit_binary(node),
            Expression::Object(node) => self.visit_object(node),
            Expression::Array(node) => self.visit_array(node),
            Expression::IndexAccess(node) => self.visit_index_access(&node),
            Expression::ForLoop(node) => self.visit_for_loop(node),
            Expression::PropertyAccess(node) => self.visit_expression(&node.object),
            Expression::Assignment(node) => self.visit_assignment(node),

            Expression::Int(..) => (),
            Expression::Bool(..) => (),
            Expression::String(..) => (),
            Expression::Builtin(..) => (),
            Expression::Empty(..) => (),
        };
    }

    fn visit_index_access(&mut self, node: &IndexAccessNode) {
        self.visit_expression(&node.indexee);
        self.visit_expression(&node.index);
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) {
        self.visit_identifier(&node.identifier);
        self.visit_expression(&node.right);
    }

    fn visit_object(&mut self, object: &ObjectNode) {
        for property in &object.properties {
            self.visit_expression(property.value.as_ref());
        }
    }

    fn visit_for_loop(&mut self, for_loop: &ForLoopNode) {
        let ForLoopNode {
            iterable,
            body,
            targets,
            ..
        } = for_loop;

        self.visit_expression(iterable);

        match targets {
            ForLoopTarget::Single(target) => {
                self.enter_scope(&vec![target.clone()]);
            }
            ForLoopTarget::Tuple(first, second) => {
                self.enter_scope(&vec![first.clone(), second.clone()]);
            }
        };

        self.visit_expression(body);

        self.exit_scope()
    }

    fn visit_array(&mut self, array: &ArrayNode) {
        for el in &array.elements {
            self.visit_expression(el);
        }
    }

    fn visit_if(&mut self, exp: &IfNode) {
        let IfNode {
            predicate,
            consequent,
            alternate,
            ..
        } = exp;

        self.visit_expression(predicate);
        self.visit_expression(consequent);

        if let Some(alternate) = alternate.as_ref() {
            self.visit_expression(alternate);
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

    fn visit_fn(&mut self, exp: &FnNode) {
        let FnNode {
            identifier,
            parameters,
            body,
            ..
        } = exp;
        self.scope.declare(&identifier.name);
        let identifiers = parameters.iter().map(|t| t.identifier.clone()).collect();
        self.enter_scope(&identifiers);
        self.visit_expression(body);
        self.exit_scope();
    }

    fn visit_block(&mut self, block: &BlockNode) {
        let BlockNode {
            body, return_val, ..
        } = block;
        for exp in body {
            self.visit_expression(exp);
        }
        self.visit_expression(return_val);
    }

    fn visit_invocation(&mut self, exp: &InvocationNode) {
        let InvocationNode {
            callee, arguments, ..
        } = exp;
        self.visit_expression(callee);
        for arg in arguments {
            self.visit_expression(arg);
        }
    }

    fn visit_identifier(&mut self, identifier: &IdentifierNode) {
        if self.scope.has_identifier(&identifier) {
            return;
        }
        self.diagnostics_list
            .add(Diagnostic::Error(ErrorDiagnostic::UndefinedReference(
                UndefinedReferenceError {
                    loc: identifier.loc,
                    name: identifier.name.to_string(),
                },
            )));
    }

    fn visit_let(&mut self, l: &LetBindingNode) {
        let LetBindingNode { right, binding, .. } = l;
        self.visit_expression(right);
        self.scope.declare(&binding.identifier.name);
    }

    fn visit_binary(&mut self, bin: &BinaryOperationNode) {
        let BinaryOperationNode { left, right, .. } = bin;
        self.visit_expression(left);
        self.visit_expression(right)
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

        st.visit_expression(&ast);
        assert!(!st.diagnostics_list.has_errors())
    }

    #[test]
    fn visit_fail() {
        let code = String::from("fn f(){5} f()");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();
        st.visit_expression(&ast);

        assert!(!st.diagnostics_list.has_errors())
    }
}
