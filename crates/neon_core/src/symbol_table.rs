use crate::{
    diagnostic::{Diagnostic, DiagnosticKind, DiagnosticLevel, DiagnosticsList},
    parser::{
        FnNode, ForLoopNode, ForLoopTarget, IdentifierNode, LetBindingNode, StructDefinitionNode,
        StructInstantiationNode,
    },
    visitor::Visitor,
};
use std::{collections::HashSet, mem};

#[derive(Debug, Clone)]
pub struct Scope {
    parent: Option<Box<Scope>>,
    variable_declarations: HashSet<String>,
    function_declarations: HashSet<String>,
    struct_declarations: HashSet<String>,
}

impl Scope {
    pub fn global() -> Scope {
        Scope {
            variable_declarations: HashSet::new(),
            function_declarations: HashSet::new(),
            struct_declarations: HashSet::new(),
            parent: None,
        }
    }

    pub fn declare_variable(&mut self, id: &str) {
        self.variable_declarations.insert(id.to_string());
    }

    pub fn declare_struct(&mut self, id: &str) {
        self.struct_declarations.insert(id.to_string());
    }

    pub fn declare_fn(&mut self, id: &str) {
        self.function_declarations.insert(id.to_string());
    }

    pub fn exit(&mut self) {
        let current = mem::replace(&mut self.parent, None);
        match current {
            Some(current) => *self = *current,
            None => panic!("Cannot exit root scope"),
        };
    }

    pub fn enter(&mut self, identifiers: &Vec<IdentifierNode>) {
        let mut declarations = HashSet::new();
        for id in identifiers {
            declarations.insert(id.to_string());
        }

        let new_parent = Scope {
            parent: self.parent.take(),
            variable_declarations: std::mem::take(&mut self.variable_declarations),
            function_declarations: std::mem::take(&mut self.function_declarations),
            struct_declarations: std::mem::take(&mut self.struct_declarations),
        };

        self.parent = Some(Box::new(new_parent));
        self.variable_declarations = declarations;
    }

    pub fn is_struct_declared(&self, name: &str) -> bool {
        if self.struct_declarations.contains(name) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.is_struct_declared(name);
        }

        false
    }

    pub fn is_fn_declared(&self, identifier: &IdentifierNode) -> bool {
        let id = &identifier.name;

        if self.function_declarations.contains(id) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.is_fn_declared(identifier);
        }

        false
    }

    pub fn is_variable_declared(&self, identifier: &IdentifierNode) -> bool {
        let id = &identifier.name;

        if self.variable_declarations.contains(id) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.is_variable_declared(identifier);
        }

        false
    }
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    dl: &'a mut DiagnosticsList,
    scope: &'a mut Scope,
}

impl Visitor for SymbolTable<'_> {
    fn enter_struct_definition(&mut self, node: &StructDefinitionNode) {
        if self.scope.is_struct_declared(&node.name.value) {
            self.dl.add(Diagnostic {
                kind: DiagnosticKind::DuplicateDefinition {
                    name: node.name.value.clone(),
                    typeof_duplicate: "struct".to_string(),
                },
                level: DiagnosticLevel::Error,
                loc: node.loc,
            });
            return;
        };

        self.scope
            .struct_declarations
            .insert(node.name.value.clone());
    }

    fn enter_struct_instantiation(&mut self, node: &StructInstantiationNode) {
        if self.scope.is_struct_declared(&node.identifier.name) {
            return;
        }
        self.undefined_reference(&node.identifier, "struct");
    }

    fn enter_for_loop(&mut self, for_loop: &ForLoopNode) {
        match &for_loop.targets {
            ForLoopTarget::Single(target) => self.scope.enter(&vec![target.clone()]),
            ForLoopTarget::Tuple(first, second) => {
                self.scope.enter(&vec![first.clone(), second.clone()])
            }
        };
    }

    fn leave_for_loop(&mut self, _for_loop: &ForLoopNode) {
        self.scope.exit();
    }

    fn enter_fn(&mut self, node: &FnNode) {
        if self.scope.is_fn_declared(&node.identifier) {
            self.dl.add(Diagnostic {
                level: DiagnosticLevel::Error,
                kind: DiagnosticKind::DuplicateDefinition {
                    typeof_duplicate: "function".to_string(),
                    name: node.identifier.name.clone(),
                },
                loc: node.loc,
            });
        };

        let FnNode {
            identifier,
            parameters,
            ..
        } = node;
        self.scope.declare_fn(&identifier.name);
        let identifiers = parameters.iter().map(|p| p.identifier.to_owned()).collect();
        self.scope.enter(&identifiers);
    }

    fn leave_fn(&mut self, _n: &FnNode) {
        self.scope.exit();
    }

    fn visit_identifier(&mut self, identifier: &IdentifierNode) {
        if self.scope.is_variable_declared(&identifier) {
            return;
        };

        if self.scope.is_fn_declared(&identifier) {
            return;
        };

        if self.scope.is_struct_declared(&identifier.name) {
            return;
        }

        self.undefined_reference(identifier, "identifier");
    }

    fn enter_let_binding(&mut self, let_binding: &LetBindingNode) {
        self.scope
            .declare_variable(&let_binding.binding.identifier.name);
    }
}

impl SymbolTable<'_> {
    pub fn new<'a>(dl: &'a mut DiagnosticsList, scope: &'a mut Scope) -> SymbolTable<'a> {
        SymbolTable { dl, scope }
    }

    fn undefined_reference(&mut self, identifier: &IdentifierNode, reference_type: &str) {
        self.dl.add(Diagnostic {
            level: DiagnosticLevel::Error,
            kind: DiagnosticKind::UndefinedReference {
                reference_type: reference_type.to_string(),
                name: identifier.name.to_string(),
            },
            loc: identifier.loc,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn visit_ok() {
        let code = String::from(
            "
let a = 3
fn t(){ 3 + 4 }
t()
a
",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut scope = Scope::global();
        let mut dl = DiagnosticsList::new();
        SymbolTable::new(&mut dl, &mut scope)
            .scanner()
            .scan_expression(&ast);
        assert!(!dl.has_errors())
    }

    #[test]
    fn visit_fail() {
        let code = String::from(
            "
fn f(){
  5
}
d()
",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut scope = Scope::global();
        let mut dl = DiagnosticsList::new();
        SymbolTable::new(&mut dl, &mut scope)
            .scanner()
            .scan_expression(&ast);
        assert!(dl.has_errors())
    }
}
