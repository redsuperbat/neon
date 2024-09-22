use core::mem::discriminant as tag;
use std::{collections::HashMap, fmt::Display};

use crate::{
    diagnostic::{Diagnostic, DiagnosticsList, ErrorDiagnostic, UnassignableTypeError},
    location::Location,
    parser::{
        AssignmentNode, BlockNode, Expression, IdentifierNode, IfNode, LetBindingNode, ObjectNode,
    },
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub struct PropertyType {
    name: String,
    value: Type,
}

#[derive(Debug, Clone)]
pub struct ObjectType {
    properties: Vec<PropertyType>,
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    elements: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct UnionType {
    variants: Vec<Type>,
}

#[derive(Debug, Clone)]
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Type::String => "string".to_string(),
            Type::Int => "int".to_string(),
            Type::Bool => "boolean".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Never => "never".to_string(),
            t => todo!("{:?}", t),
        };
        write!(f, "{name}")
    }
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

impl TypeEnvironment {
    pub fn new(symbol_table: SymbolTable) -> TypeEnvironment {
        TypeEnvironment {
            symbol_table,
            bindings: HashMap::new(),
        }
    }
}

pub struct TypeChecker<'a> {
    diagnostics_list: &'a mut DiagnosticsList,
    current_loc: Location,
}

impl TypeChecker<'_> {
    pub fn new(diagnostics_list: &mut DiagnosticsList) -> TypeChecker {
        TypeChecker {
            diagnostics_list,
            current_loc: Location::beginning(),
        }
    }

    pub fn typeof_expression(
        &mut self,
        expression: &Expression,
        env: &mut TypeEnvironment,
    ) -> Type {
        self.current_loc = expression.loc();
        match &expression {
            Expression::Assignment(node) => self.typeof_assignment(node, env),
            Expression::Block(node) => self.typeof_block(node, env),
            Expression::Identifier(node) => self.typeof_identifier(node, env),
            Expression::If(node) => self.typeof_if(node, env),
            Expression::LetBinding(node) => self.typeof_let_binding(node, env),
            Expression::Object(node) => self.typeof_object(node, env),

            Expression::Bool(..) => Type::Bool,
            Expression::Empty(..) => Type::Unit,
            Expression::Int(..) => Type::Int,
            Expression::String(..) => Type::String,
            e => todo!("{:?}", e),
        }
    }

    fn typeof_block(&mut self, node: &BlockNode, env: &mut TypeEnvironment) -> Type {
        for e in &node.body {
            self.typeof_expression(e, env);
        }
        self.typeof_expression(&node.return_val, env)
    }

    fn typeof_let_binding(&mut self, node: &LetBindingNode, env: &mut TypeEnvironment) -> Type {
        let rhs = self.typeof_expression(&node.right, env);
        env.bindings.insert(node.name.clone(), rhs);
        Type::Unit
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

    fn typeof_assignment(&mut self, node: &AssignmentNode, env: &mut TypeEnvironment) -> Type {
        let lhs = self.typeof_identifier(&node.identifier, env);
        let rhs = self.typeof_expression(&node.right, env);
        self.unify(lhs, rhs);
        Type::Unit
    }

    fn typeof_if(&mut self, node: &IfNode, env: &mut TypeEnvironment) -> Type {
        let typeof_predicate = self.typeof_expression(&node.predicate, env);
        if typeof_predicate != Type::Bool {
            return Type::Never;
        };
        return Type::Never;
    }

    fn typeof_identifier(&mut self, node: &IdentifierNode, env: &mut TypeEnvironment) -> Type {
        match env.bindings.get(&node.name) {
            Some(t) => t.clone(),
            None => Type::Never,
        }
    }

    fn type_error(&mut self, lhs: Type, rhs: Type) -> Type {
        self.diagnostics_list
            .add(Diagnostic::Error(ErrorDiagnostic::UnassignableType(
                UnassignableTypeError {
                    loc: self.current_loc,
                    lhs,
                    rhs,
                },
            )));
        Type::Never
    }

    fn unify(&mut self, lhs: Type, rhs: Type) -> Type {
        match lhs {
            Type::Int => match rhs {
                Type::Int => Type::Int,
                _ => self.type_error(lhs, rhs),
            },
            Type::Bool => match rhs {
                Type::Bool => Type::Bool,
                _ => self.type_error(lhs, rhs),
            },
            Type::String => match rhs {
                Type::String => Type::String,
                _ => self.type_error(lhs, rhs),
            },

            Type::Object(_) => todo!(),
            Type::Array(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Unit => todo!(),
            _ => Type::Never,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn typeof_ok() {
        let code = String::from(
            "
let a = 3 
a = true
",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();
        let _ = st.visit_expression(&ast);
        let mut dl = DiagnosticsList::new();
        let mut ts = TypeChecker::new(&mut dl);
        let mut env = TypeEnvironment {
            bindings: HashMap::new(),
            symbol_table: st,
        };

        let t = ts.typeof_expression(&ast, &mut env);
        println!("{:?}", dl);
        println!("{:?}", t);
    }
}
