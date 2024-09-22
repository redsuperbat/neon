use core::mem::discriminant as tag;
use std::{collections::HashMap, fmt::Display};

use crate::{
    diagnostic::{
        Diagnostic, DiagnosticsList, ErrorDiagnostic, IncompatibleTypesError, UnassignableTypeError,
    },
    location::Location,
    parser::{
        ArrayNode, AssignmentNode, BinaryOperationNode, BlockNode, ElseNode, Expression, FnNode,
        ForLoopNode, IdentifierNode, IfNode, InvocationNode, LetBindingNode, ObjectNode,
        PropertyAccessNode,
    },
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
    Any,
    String,
    Unit,
    TypeFn,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Type::String => "string".to_string(),
            Type::Int => "int".to_string(),
            Type::Bool => "boolean".to_string(),
            Type::Unit => "unit".to_string(),
            Type::Never => "never".to_string(),
            Type::Any => "any".to_string(),
            Type::TypeFn => "fn".to_string(),
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
    pub bindings: HashMap<String, Type>,
}

impl TypeEnvironment {
    pub fn new() -> TypeEnvironment {
        TypeEnvironment {
            bindings: HashMap::new(),
        }
    }
}

pub struct TypeChecker {
    pub diagnostics_list: DiagnosticsList,
    current_loc: Location,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            diagnostics_list: DiagnosticsList::new(),
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
            Expression::Binary(node) => self.typeof_binary(node, env),
            Expression::Invocation(node) => self.typeof_invocation(node, env),
            Expression::Else(node) => self.typeof_else(node, env),
            Expression::Fn(node) => self.typeof_fn(node, env),
            Expression::ForLoop(node) => self.typeof_for_loop(node, env),
            Expression::Array(node) => self.typeof_array(node, env),
            Expression::PropertyAccess(node) => self.typeof_property_access(node, env),

            Expression::Bool(..) => Type::Bool,
            Expression::Empty(..) => Type::Unit,
            Expression::Int(..) => Type::Int,
            Expression::String(..) => Type::String,
            e => todo!("{:#?}", e),
        }
    }

    fn typeof_property_access(
        &mut self,
        node: &PropertyAccessNode,
        env: &mut TypeEnvironment,
    ) -> Type {
        Type::Any
    }

    fn typeof_array(&mut self, node: &ArrayNode, env: &mut TypeEnvironment) -> Type {
        Type::Any
    }

    fn typeof_else(&mut self, node: &ElseNode, env: &mut TypeEnvironment) -> Type {
        self.typeof_expression(&node.consequent, env)
    }

    fn typeof_for_loop(&mut self, node: &ForLoopNode, env: &mut TypeEnvironment) -> Type {
        self.typeof_expression(&node.body, env);
        Type::Any
    }

    fn typeof_fn(&mut self, node: &FnNode, env: &mut TypeEnvironment) -> Type {
        for param in &node.parameters {
            // TODO: Fix
            self.typeof_identifier(param, env);
        }
        self.typeof_expression(&node.body, env);
        Type::Any
    }

    fn typeof_invocation(&mut self, node: &InvocationNode, env: &mut TypeEnvironment) -> Type {
        let callee = self.typeof_expression(&node.callee, env);
        return self.unify(callee, Type::TypeFn);
    }

    fn typeof_binary(&mut self, node: &BinaryOperationNode, env: &mut TypeEnvironment) -> Type {
        let lhs = self.typeof_expression(&node.left, env);
        let rhs = self.typeof_expression(&node.right, env);
        self.unify(lhs, rhs)
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
        // TODO: We should do a better check here to make sure that the predicate resolves to a
        // boolean type
        self.typeof_expression(&node.predicate, env);

        let Some(alternate) = node.alternate.as_ref() else {
            return Type::Unit;
        };

        let consequent = self.typeof_expression(&node.consequent, env);
        let alternate = self.typeof_expression(&alternate, env);

        if consequent != alternate {
            println!("{},{}, {:#?}", consequent, alternate, node);
            return self.add_error_diagnostic(ErrorDiagnostic::IncompatibleTypes(
                IncompatibleTypesError {
                    loc: node.loc,
                    alternate,
                    consequent,
                },
            ));
        }
        consequent
    }

    fn typeof_identifier(&mut self, node: &IdentifierNode, env: &mut TypeEnvironment) -> Type {
        match env.bindings.get(&node.name) {
            Some(t) => t.clone(),
            None => Type::Never,
        }
    }

    fn add_error_diagnostic(&mut self, d: ErrorDiagnostic) -> Type {
        self.diagnostics_list.add(Diagnostic::Error(d));
        Type::Never
    }

    fn type_error(&mut self, lhs: Type, rhs: Type) -> Type {
        self.add_error_diagnostic(ErrorDiagnostic::UnassignableType(UnassignableTypeError {
            loc: self.current_loc,
            lhs,
            rhs,
        }));
        Type::Never
    }

    fn unify(&mut self, lhs: Type, rhs: Type) -> Type {
        if rhs == Type::Any || lhs == Type::Any {
            return Type::Any;
        }

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
        let mut ts = TypeChecker::new();
        let mut env = TypeEnvironment {
            bindings: HashMap::new(),
        };

        let t = ts.typeof_expression(&ast, &mut env);
        println!("{:?}", ts.diagnostics_list);
        println!("{:?}", t);
    }
}
