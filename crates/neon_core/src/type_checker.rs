use core::mem::discriminant as tag;
use std::collections::HashMap;

use crate::{
    diagnostic::{Diagnostic, DiagnosticsList, ErrorDiagnostic},
    location::Location,
    parser::{BoolNode, Expression, IdentifierNode, IfNode, IntNode, ObjectNode, StringNode},
    symbol_table::SymbolTable,
};

pub struct PropertyType {
    name: String,
    value: Type,
}

pub struct ObjectType {
    properties: Vec<PropertyType>,
}

pub struct ArrayType {
    elements: Box<Type>,
}

pub struct UnionType {
    variants: Vec<Type>,
}

pub enum Type {
    Never,
    Int,
    Bool,
    Object(ObjectType),
    Array(ArrayType),
    Union(UnionType),
    String,
    Unit,
    TypeFn {
        inputs: Vec<Type>,
        outputs: Box<Type>,
    },
}

impl PartialEq<Self> for Type {
    fn eq(&self, other: &Self) -> bool {
        tag(self) == tag(other)
    }
}

pub struct TypeEnvironment {
    pub symbol_table: SymbolTable,
    pub bindings: HashMap<String, Type>,
}

#[derive(Debug)]
pub struct TypeChecker<'a> {
    diagnostics_list: &'a mut DiagnosticsList,
    current_loc: Location,
}

impl TypeChecker<'_> {
    pub fn typeof_expression(
        &mut self,
        expression: &Expression,
        env: &mut TypeEnvironment,
    ) -> Type {
        self.current_loc = expression.loc();
        match &expression {
            Expression::Fn(..) => self.typeof_fn(expression, env),
            Expression::Identifier(node) => self.typeof_identifier(node, env),
            Expression::Invocation(..) => self.typeof_invocation(expression, env),
            Expression::LetBinding { .. } => self.typeof_let(expression),
            Expression::Block { .. } => self.typeof_block(expression, env),
            Expression::If(node) => self.typeof_if(node, env),
            Expression::Else { .. } => self.typeof_expression(expression, env),

            Expression::Binary { .. } => self.typeof_binary(expression, env),
            Expression::Array { .. } => self.typeof_array(expression),
            Expression::IndexAccess { .. } => self.typeof_index_access(expression),
            Expression::Builtin { .. } => self.typeof_builtin(expression),

            Expression::ForLoop { .. } => todo!(),
            Expression::PropertyAccess { .. } => todo!(),
            Expression::Object(node) => self.typeof_object(node, env),

            Expression::Int(..) => Type::Int,
            Expression::Bool(..) => Type::Bool,
            Expression::String(..) => Type::String,
            Expression::Empty(..) => Type::Unit,
            _ => todo!(),
        }
    }

    fn typeof_object(&mut self, node: &ObjectNode, env: &mut TypeEnvironment) -> Type {
        let properties = node
            .properties
            .iter()
            .map(|n| PropertyType {
                name: n.identifier.name.clone(),
                value: self.typeof_expression(&n.value, env),
            })
            .collect::<Vec<_>>();
        Type::Object(ObjectType { properties })
    }

    fn typeof_builtin(&mut self, exp: &Expression) -> Type {
        todo!();
    }

    fn typeof_index_access(&mut self, exp: &Expression) -> Type {
        todo!();
    }

    fn typeof_array(&mut self, exp: &Expression) -> Type {
        todo!()
    }

    fn typeof_if(&mut self, node: &IfNode, env: &mut TypeEnvironment) -> Type {
        let typeof_predicate = self.typeof_expression(&node.predicate, env);
        if typeof_predicate != Type::Bool {
            self.diagnostics_list
                .add(Diagnostic::Error(ErrorDiagnostic { loc: node.loc }));
            return Type::Never;
        };
        return Type::Never;
    }

    fn typeof_fn(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!();
    }

    fn typeof_block(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_invocation(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_identifier(&mut self, node: &IdentifierNode, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_let(&mut self, exp: &Expression) -> Type {
        todo!()
    }

    fn typeof_binary(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn unify(&mut self, lhs: Type, rhs: Type) -> Type {
        if lhs == Type::Never {
            self.diagnostics_list
                .add(Diagnostic::Error(ErrorDiagnostic {
                    loc: self.current_loc,
                }));
            return lhs;
        }
        return Type::Never;
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
