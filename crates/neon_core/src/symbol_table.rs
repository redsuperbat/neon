use crate::{
    diagnostic::{Diagnostic, DiagnosticsList, ErrorDiagnostic, UndefinedReferenceError},
    parser::{
        ArrayNode, AssignmentNode, BinaryOperationNode, BlockNode, Expression, FnNode, ForLoopNode,
        ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode, InvocationNode, LetBindingNode,
        ObjectInstantiationNode,
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

    pub fn exit(&mut self) {
        let current = mem::replace(&mut self.parent, None);
        match current {
            Some(current) => *self = *current,
            None => panic!("Internal neon error"),
        };
    }

    pub fn enter(&mut self, identifiers: &Vec<IdentifierNode>) {
        let mut declarations = HashSet::new();
        for id in identifiers {
            declarations.insert(id.to_string());
        }

        let new_parent = Scope {
            parent: self.parent.take(),
            declarations: std::mem::take(&mut self.declarations),
        };

        self.parent = Some(Box::new(new_parent));
        self.declarations = declarations;
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
pub struct SymbolTable<'a> {
    dl: &'a mut DiagnosticsList,
}

impl SymbolTable<'_> {
    pub fn new<'a>(dl: &'a mut DiagnosticsList) -> SymbolTable<'a> {
        SymbolTable { dl }
    }

    pub fn visit_expression(&mut self, expression: &Expression, s: &mut Scope) {
        match &expression {
            Expression::Fn(node) => self.visit_fn(node, s),
            Expression::Identifier(node) => self.visit_identifier(node, s),
            Expression::Invocation(node) => self.visit_invocation(node, s),
            Expression::LetBinding(node) => self.visit_let(node, s),
            Expression::Block(node) => self.visit_block(node, s),
            Expression::If(node) => self.visit_if(node, s),
            Expression::Binary(node) => self.visit_binary(node, s),
            Expression::Array(node) => self.visit_array(node, s),
            Expression::ForLoop(node) => self.visit_for_loop(node, s),
            Expression::ObjectInstantiation(node) => self.visit_object(node, s),
            Expression::Else(node) => self.visit_expression(&node.consequent, s),
            Expression::IndexAccess(node) => self.visit_index_access(&node, s),
            Expression::PropertyAccess(node) => self.visit_expression(&node.object, s),
            Expression::Assignment(node) => self.visit_assignment(node, s),

            Expression::Use(..) => (),
            Expression::Int(..) => (),
            Expression::Bool(..) => (),
            Expression::String(..) => (),
            Expression::Builtin(..) => (),
            Expression::Empty(..) => (),
        };
    }

    fn visit_index_access(&mut self, node: &IndexAccessNode, s: &mut Scope) {
        self.visit_expression(&node.indexee, s);
        self.visit_expression(&node.index, s);
    }

    fn visit_assignment(&mut self, node: &AssignmentNode, s: &mut Scope) {
        self.visit_identifier(&node.identifier, s);
        self.visit_expression(&node.right, s);
    }

    fn visit_object(&mut self, object: &ObjectInstantiationNode, s: &mut Scope) {
        s.enter(&vec![]);
        for property in &object.properties {
            self.visit_expression(property.value.as_ref(), s);
        }
        s.exit();
    }

    fn visit_for_loop(&mut self, for_loop: &ForLoopNode, s: &mut Scope) {
        let ForLoopNode {
            iterable,
            body,
            targets,
            ..
        } = for_loop;

        self.visit_expression(iterable, s);

        match targets {
            ForLoopTarget::Single(target) => s.enter(&vec![target.clone()]),
            ForLoopTarget::Tuple(first, second) => s.enter(&vec![first.clone(), second.clone()]),
        };

        self.visit_expression(body, s);

        s.exit();
    }

    fn visit_array(&mut self, array: &ArrayNode, s: &mut Scope) {
        for el in &array.elements {
            self.visit_expression(el, s);
        }
    }

    fn visit_if(&mut self, exp: &IfNode, s: &mut Scope) {
        let IfNode {
            predicate,
            consequent,
            alternate,
            ..
        } = exp;

        self.visit_expression(predicate, s);
        self.visit_expression(consequent, s);

        if let Some(alternate) = alternate.as_ref() {
            self.visit_expression(alternate, s);
        }
    }

    fn visit_fn(&mut self, exp: &FnNode, s: &mut Scope) {
        let FnNode {
            identifier,
            parameters,
            body,
            ..
        } = exp;

        s.declare(&identifier.name);
        let identifiers = parameters.iter().map(|t| t.identifier.clone()).collect();
        s.enter(&identifiers);
        self.visit_expression(body, s);
        s.exit();
    }

    fn visit_block(&mut self, block: &BlockNode, s: &mut Scope) {
        let BlockNode {
            body, return_val, ..
        } = block;
        for exp in body {
            self.visit_expression(exp, s);
        }
        self.visit_expression(return_val, s);
    }

    fn visit_invocation(&mut self, exp: &InvocationNode, s: &mut Scope) {
        let InvocationNode {
            callee, arguments, ..
        } = exp;
        self.visit_expression(callee, s);
        for arg in arguments {
            self.visit_expression(arg, s);
        }
    }

    fn visit_identifier(&mut self, identifier: &IdentifierNode, s: &mut Scope) {
        if s.has_identifier(&identifier) {
            return;
        }
        self.dl
            .add(Diagnostic::Error(ErrorDiagnostic::UndefinedReference(
                UndefinedReferenceError {
                    loc: identifier.loc,
                    name: identifier.name.to_string(),
                },
            )));
    }

    fn visit_let(&mut self, l: &LetBindingNode, s: &mut Scope) {
        let LetBindingNode { right, binding, .. } = l;
        if let Some(right) = right.as_ref() {
            self.visit_expression(right, s);
        };
        s.declare(&binding.identifier.name);
    }

    fn visit_binary(&mut self, bin: &BinaryOperationNode, s: &mut Scope) {
        let BinaryOperationNode { left, right, .. } = bin;
        self.visit_expression(left, s);
        self.visit_expression(right, s)
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
        let mut scope = Scope::global();
        let mut dl = DiagnosticsList::new();
        let mut st = SymbolTable::new(&mut dl);
        st.visit_expression(&ast, &mut scope);
        assert!(!dl.has_errors())
    }

    #[test]
    fn visit_fail() {
        let code = String::from("fn f(){5} f()");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut scope = Scope::global();
        let mut dl = DiagnosticsList::new();
        let mut st = SymbolTable::new(&mut dl);

        st.visit_expression(&ast, &mut scope);

        assert!(!dl.has_errors())
    }
}
