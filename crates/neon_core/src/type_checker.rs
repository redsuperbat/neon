use core::mem::discriminant as tag;
use std::{collections::HashMap, fmt::Display};

use crate::{
    diagnostic::{
        Diagnostic, DiagnosticsList, ErrorDiagnostic, ExpressionNotInvokableError,
        IncompatibleTypesError, PropertyDoesNotExistError, UnassignableTypeError,
    },
    location::Location,
    parser::{
        ArrayNode, AssignmentNode, BinaryOp, BinaryOperationNode, BlockNode, BuiltinExpressionKind,
        ElseNode, Expression, FnNode, ForLoopNode, IdentifierNode, IfNode, InvocationNode,
        LetBindingNode, ObjectNode, PropertyAccessNode,
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
pub struct FnType {
    parameters: Vec<Type>,
    return_type: Box<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Never,
    Int,
    Bool,
    Object(ObjectType),
    Array(ArrayType),
    Fn(FnType),

    String,
    Unit,
    Any,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn fmt_type(depth: usize, t: &Type) -> String {
            match t {
                Type::String => "string".to_string(),
                Type::Int => "int".to_string(),
                Type::Bool => "boolean".to_string(),
                Type::Unit => "unit".to_string(),
                Type::Never => "never".to_string(),
                Type::Any => "any".to_string(),
                Type::Fn(FnType {
                    parameters,
                    return_type,
                }) => {
                    let parameters = parameters
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",");

                    format!("{parameters} -> {}", return_type)
                }
                Type::Array(t) => format!("{}[]", t.elements),
                Type::Object(ObjectType { properties }) => {
                    let mut result = String::from("{\n");
                    let indentation = " ".repeat(depth);

                    for (i, PropertyType { name, value }) in properties.iter().enumerate() {
                        if i != 0 {
                            result += ",\n"
                        }
                        result += &format!("{}{name}: {}", indentation, fmt_type(depth + 1, value));
                    }

                    result += &format!("\n{indentation}}}");
                    result
                }
            }
        }

        write!(f, "{}", fmt_type(0, self))
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

impl TypeEnvironment {
    pub fn register_bultin(&mut self, kind: &BuiltinExpressionKind) {
        let parameters = (0..=100).map(|_| Type::Any).collect();
        let name = kind.name();
        let fn_type = FnType {
            return_type: Box::new(Type::Any),
            parameters,
        };

        self.bindings.insert(name, Type::Fn(fn_type));
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
        let access_type = self.typeof_expression(&node.object, env);
        let error = PropertyDoesNotExistError {
            loc: node.loc,
            access_type,
            key: node.property_name.name.clone(),
        };
        let Type::Object(ObjectType { properties }) = self.typeof_expression(&node.object, env)
        else {
            return self.add_error_diagnostic(ErrorDiagnostic::PropertyDoesNotExist(error));
        };

        let prop_type = properties
            .iter()
            .find(|p| p.name == node.property_name.name);

        let Some(prop_type) = prop_type else {
            return self.add_error_diagnostic(ErrorDiagnostic::PropertyDoesNotExist(error));
        };

        prop_type.value.clone()
    }

    fn typeof_array(&mut self, _node: &ArrayNode, _env: &mut TypeEnvironment) -> Type {
        Type::Array(ArrayType {
            elements: Box::new(Type::Any),
        })
    }

    fn typeof_else(&mut self, node: &ElseNode, env: &mut TypeEnvironment) -> Type {
        self.typeof_expression(&node.consequent, env)
    }

    fn typeof_for_loop(&mut self, node: &ForLoopNode, env: &mut TypeEnvironment) -> Type {
        self.typeof_expression(&node.body, env);
        Type::Unit
    }

    fn typeof_fn(&mut self, node: &FnNode, env: &mut TypeEnvironment) -> Type {
        let parameters = node.parameters.iter().map(|_| Type::Any).collect();
        let return_type = self.typeof_expression(&node.body, env);
        let fn_type = Type::Fn(FnType {
            parameters,
            return_type: Box::new(return_type),
        });
        env.bindings.insert(node.name.clone(), fn_type.clone());
        fn_type
    }

    fn typeof_invocation(&mut self, node: &InvocationNode, env: &mut TypeEnvironment) -> Type {
        let callee = self.typeof_expression(&node.callee, env);

        let Type::Fn(fn_type) = callee else {
            return self.add_error_diagnostic(ErrorDiagnostic::ExpressionNotInvokable(
                ExpressionNotInvokableError {
                    loc: node.callee.loc(),
                },
            ));
        };

        for (param, arg) in fn_type.parameters.iter().zip(node.arguments.iter()) {
            let arg_type = self.typeof_expression(arg, env);
            self.unify(param, &arg_type);
        }
        *fn_type.return_type
    }

    fn typeof_binary(&mut self, node: &BinaryOperationNode, env: &mut TypeEnvironment) -> Type {
        let lhs = self.typeof_expression(&node.left, env);
        let rhs = self.typeof_expression(&node.right, env);
        let unification = self.unify(&lhs, &rhs);

        match node.operation {
            BinaryOp::Mod | BinaryOp::Add | BinaryOp::Sub => unification,

            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Ne
            | BinaryOp::Eq => Type::Bool,
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
        self.current_loc = node.loc;
        self.unify(&lhs, &rhs);
        Type::Unit
    }

    fn typeof_if(&mut self, node: &IfNode, env: &mut TypeEnvironment) -> Type {
        let predicate = self.typeof_expression(&node.predicate, env);
        self.current_loc = node.predicate.loc();
        self.unify(&Type::Bool, &predicate);

        let Some(alternate) = node.alternate.as_ref() else {
            return Type::Unit;
        };

        let consequent = self.typeof_expression(&node.consequent, env);
        let alternate = self.typeof_expression(&alternate, env);

        if consequent != alternate {
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

    fn type_error(&mut self, lhs: &Type, rhs: &Type) -> Type {
        self.add_error_diagnostic(ErrorDiagnostic::UnassignableType(UnassignableTypeError {
            loc: self.current_loc,
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        }));
        Type::Never
    }

    fn unify(&mut self, lhs: &Type, rhs: &Type) -> Type {
        if *rhs == Type::Any || *lhs == Type::Any {
            return Type::Any;
        }

        match &lhs {
            Type::Int => match &rhs {
                Type::Int => Type::Int,
                _ => self.type_error(&lhs, rhs),
            },
            Type::Bool => match &rhs {
                Type::Bool => Type::Bool,
                _ => self.type_error(lhs, rhs),
            },
            Type::String => match &rhs {
                Type::String => Type::String,
                _ => self.type_error(lhs, rhs),
            },
            Type::Unit => match &rhs {
                Type::Unit => Type::Unit,
                _ => self.type_error(lhs, rhs),
            },
            Type::Object(a) => match &rhs {
                Type::Object(b) => {
                    for PropertyType { name, value } in &a.properties {
                        let b_prop = b.properties.iter().find(|p| p.name == *name);
                        let Some(b_prop) = b_prop else {
                            return self.type_error(lhs, rhs);
                        };
                        self.unify(value, &b_prop.value);
                    }

                    lhs.clone()
                }

                _ => self.type_error(lhs, rhs),
            },
            Type::Array(a) => match &rhs {
                Type::Array(b) => self.unify(&a.elements, &b.elements),
                _ => self.type_error(lhs, rhs),
            },
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
