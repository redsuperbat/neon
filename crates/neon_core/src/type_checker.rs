use crate::{
    diagnostic::{
        Diagnostic, DiagnosticsList, ErrorDiagnostic, ExpressionNotInvokableError,
        IncompatibleTypesError, InsufficientArgumentsError, InvalidIndexAccessError,
        NotIterableError, PropertyDoesNotExistError, UnassignableTypeError, UndefinedTypeError,
    },
    location::{Location, WithLocation},
    parser::{
        ArrayNode, AssignmentNode, BinaryOp, BinaryOperationNode, BlockNode, BuiltinNode, ElseNode,
        Expression, FnNode, ForLoopNode, ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode,
        InvocationNode, LetBindingNode, ObjectNode, PropertyAccessNode, TypeExpression,
    },
};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyType {
    name: String,
    value: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectType {
    properties: Vec<PropertyType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    elements: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    parameters: Vec<Type>,
    return_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
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
                    ..
                }) => {
                    let parameters = parameters
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",");

                    format!("({parameters}) -> {}", return_type)
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

impl TypeEnvironment {}

pub struct TypeChecker {
    current_loc: Location,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            current_loc: Location::beginning(),
        }
    }

    pub fn typeof_expression(
        &mut self,
        expression: &Expression,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        self.current_loc = expression.loc();
        match &expression {
            Expression::Assignment(node) => self.typeof_assignment(node, env, dl),
            Expression::Block(node) => self.typeof_block(node, env, dl),
            Expression::Identifier(node) => self.typeof_identifier(node, env),
            Expression::If(node) => self.typeof_if(node, env, dl),
            Expression::LetBinding(node) => self.typeof_let_binding(node, env, dl),
            Expression::Object(node) => self.typeof_object(node, env, dl),
            Expression::Binary(node) => self.typeof_binary(node, env, dl),
            Expression::Invocation(node) => self.typeof_invocation(node, env, dl),
            Expression::Else(node) => self.typeof_else(node, env, dl),
            Expression::Fn(node) => self.typeof_fn(node, env, dl),
            Expression::ForLoop(node) => self.typeof_for_loop(node, env, dl),
            Expression::Array(node) => self.typeof_array(node, env, dl),
            Expression::PropertyAccess(node) => self.typeof_property_access(node, env, dl),
            Expression::IndexAccess(node) => self.typeof_index_access(node, env, dl),
            Expression::Builtin(node) => self.typeof_builtin(node, env),

            Expression::Bool(..) => Type::Bool,
            Expression::Empty(..) => Type::Unit,
            Expression::Int(..) => Type::Int,
            Expression::String(..) => Type::String,
        }
    }

    fn typeof_builtin(&mut self, _node: &BuiltinNode, _env: &mut TypeEnvironment) -> Type {
        Type::Any
    }

    fn typeof_index_access(
        &mut self,
        node: &IndexAccessNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let typeof_index = self.typeof_expression(&node.index, env, dl);
        self.unify(&Type::Int, &typeof_index, dl);
        let typeof_indexee = self.typeof_expression(&node.indexee, env, dl);
        let Type::Array(array_type) = typeof_indexee else {
            return self.add_error_diagnostic(
                ErrorDiagnostic::InvalidIndexAccess(InvalidIndexAccessError {
                    loc: node.index.loc(),
                    indexee_type: typeof_indexee,
                    index_type: typeof_index,
                }),
                dl,
            );
        };

        *array_type.elements
    }

    fn typeof_property_access(
        &mut self,
        node: &PropertyAccessNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let access_type = self.typeof_expression(&node.object, env, dl);
        let error = PropertyDoesNotExistError {
            access_type,
            loc: node.identifier.loc,
            key: node.identifier.name.clone(),
        };

        let Type::Object(ObjectType { properties }) = self.typeof_expression(&node.object, env, dl)
        else {
            return self.add_error_diagnostic(ErrorDiagnostic::PropertyDoesNotExist(error), dl);
        };

        let prop_type = properties.iter().find(|p| p.name == node.identifier.name);

        let Some(prop_type) = prop_type else {
            return self.add_error_diagnostic(ErrorDiagnostic::PropertyDoesNotExist(error), dl);
        };

        prop_type.value.clone()
    }

    fn typeof_array(
        &mut self,
        node: &ArrayNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let arr_types = node
            .elements
            .iter()
            .map(|e| self.typeof_expression(e, env, dl))
            .collect::<Vec<_>>();

        // TODO: When union types are implemented fix this
        let arr_type = arr_types.first().map_or(Type::Any, |first| {
            if arr_types.iter().all(|t| t == first) {
                first.clone()
            } else {
                Type::Any
            }
        });

        Type::Array(ArrayType {
            elements: Box::new(arr_type),
        })
    }

    fn typeof_else(
        &mut self,
        node: &ElseNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        self.typeof_expression(&node.consequent, env, dl)
    }

    fn typeof_for_loop(
        &mut self,
        node: &ForLoopNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let typeof_iterable = self.typeof_expression(&node.iterable, env, dl);

        match typeof_iterable {
            Type::Object(..) => match &node.targets {
                ForLoopTarget::Single(node) => {
                    // TODO: when union type is implemented use here
                    env.bindings.insert(node.name.clone(), Type::Any);
                }
                ForLoopTarget::Tuple(a, b) => {
                    env.bindings.insert(a.name.clone(), Type::String);
                    env.bindings.insert(b.name.clone(), Type::Any);
                }
            },
            Type::Array(array_type) => match &node.targets {
                ForLoopTarget::Single(node) => {
                    env.bindings.insert(node.name.clone(), *array_type.elements);
                }
                ForLoopTarget::Tuple(a, b) => {
                    env.bindings.insert(a.name.clone(), Type::Int);
                    env.bindings.insert(b.name.clone(), *array_type.elements);
                }
            },
            Type::String => match &node.targets {
                ForLoopTarget::Single(node) => {
                    env.bindings.insert(node.name.clone(), Type::String);
                }
                ForLoopTarget::Tuple(a, b) => {
                    env.bindings.insert(a.name.clone(), Type::Int);
                    env.bindings.insert(b.name.clone(), Type::String);
                }
            },
            _ => {
                return self.add_error_diagnostic(
                    ErrorDiagnostic::NotIterable(NotIterableError {
                        loc: node.iterable.loc(),
                        non_iterable: typeof_iterable,
                    }),
                    dl,
                )
            }
        }

        self.typeof_expression(&node.body, env, dl);
        Type::Unit
    }

    fn typeof_fn(
        &mut self,
        node: &FnNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let mut parameters = vec![];
        for param in &node.parameters {
            let param_type = self.typeof_type_expression(&param.typed, env, dl);
            env.bindings
                .insert(param.identifier.name.clone(), param_type.clone());
            parameters.push(param_type);
        }

        let return_type = match &node.return_type {
            Some(t) => self.typeof_type_expression(&t, env, dl),
            None => Type::Unit,
        };

        let fn_type = Type::Fn(FnType {
            parameters,
            return_type: Box::new(return_type.clone()),
        });

        env.bindings
            .insert(node.identifier.name.clone(), fn_type.clone());

        let inferred_return_type = self.typeof_expression(&node.body, env, dl);

        self.unify(&return_type, &inferred_return_type, dl);

        fn_type
    }

    fn typeof_invocation(
        &mut self,
        node: &InvocationNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let callee = self.typeof_expression(&node.callee, env, dl);

        let Type::Fn(fn_type) = callee else {
            return self.add_error_diagnostic(
                ErrorDiagnostic::ExpressionNotInvokable(ExpressionNotInvokableError {
                    loc: node.callee.loc(),
                    callee_type: callee,
                }),
                dl,
            );
        };

        for (i, param) in fn_type.parameters.iter().enumerate() {
            let arg = node.arguments.get(i);
            let Some(arg) = arg else {
                return self.add_error_diagnostic(
                    ErrorDiagnostic::InsufficientArguments(InsufficientArgumentsError {
                        loc: node.loc,
                        expected: fn_type.parameters.len(),
                        got: node.arguments.len(),
                    }),
                    dl,
                );
            };
            let arg_type = self.typeof_expression(arg, env, dl);
            self.unify(param, &arg_type, dl);
        }
        *fn_type.return_type
    }

    fn typeof_binary(
        &mut self,
        node: &BinaryOperationNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let lhs = self.typeof_expression(&node.left, env, dl);
        let rhs = self.typeof_expression(&node.right, env, dl);
        self.current_loc = node.left.loc();
        let unification = self.unify(&lhs, &rhs, dl);

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

    fn typeof_block(
        &mut self,
        node: &BlockNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        for e in &node.body {
            self.typeof_expression(e, env, dl);
        }
        self.typeof_expression(&node.return_val, env, dl)
    }

    fn typeof_type_expression(
        &mut self,
        node: &TypeExpression,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        match node {
            TypeExpression::Int(..) => Type::Int,
            TypeExpression::String(..) => Type::String,
            TypeExpression::Bool(..) => Type::Bool,
            TypeExpression::Unit(..) => Type::Unit,
            TypeExpression::Fn(node) => Type::Fn(FnType {
                return_type: Box::new(self.typeof_type_expression(&node.return_type, env, dl)),
                parameters: node
                    .parameters
                    .iter()
                    .map(|t| self.typeof_type_expression(t, env, dl))
                    .collect(),
            }),
            TypeExpression::Array(node) => Type::Array(ArrayType {
                elements: Box::new(self.typeof_type_expression(&node.elements, env, dl)),
            }),
            TypeExpression::Object(node) => Type::Object(ObjectType {
                properties: node
                    .properties
                    .iter()
                    .map(|p| PropertyType {
                        name: p.name.value.clone(),
                        value: self.typeof_type_expression(&p.property_type, env, dl),
                    })
                    .collect(),
            }),
            TypeExpression::Identifier(node) => match env.bindings.get(&node.name) {
                Some(t) => t.clone(),
                None => self.add_error_diagnostic(
                    ErrorDiagnostic::UndefinedType(UndefinedTypeError {
                        loc: node.loc,
                        name: node.name.clone(),
                    }),
                    dl,
                ),
            },
        }
    }

    fn typeof_let_binding(
        &mut self,
        node: &LetBindingNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let rhs = if let Some(right) = &node.right.as_ref() {
            self.typeof_expression(right, env, dl)
        } else {
            Type::Unit
        };

        let t = if let Some(type_exp) = &node.binding.typed {
            let lhs = self.typeof_type_expression(&type_exp, env, dl);
            self.unify(&lhs, &rhs, dl);
            lhs
        } else {
            rhs
        };

        env.bindings.insert(node.binding.identifier.name.clone(), t);
        Type::Unit
    }

    fn typeof_object(
        &mut self,
        node: &ObjectNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let properties = node
            .properties
            .iter()
            .map(|n| PropertyType {
                name: n.name.value.clone(),
                value: self.typeof_expression(&n.value, env, dl),
            })
            .collect::<Vec<_>>();
        Type::Object(ObjectType { properties })
    }

    fn typeof_assignment(
        &mut self,
        node: &AssignmentNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let mut lhs = self.typeof_identifier(&node.identifier, env);
        let rhs = self.typeof_expression(&node.right, env, dl);
        // We do this to make the error appear on the left part of the assignment
        self.current_loc = node.identifier.loc;

        // If the lhs is unit we give it the type of the rhs
        if lhs == Type::Unit {
            lhs = rhs.clone();
        };

        let t = self.unify(&lhs, &rhs, dl);
        if t != Type::Never {
            env.bindings.insert(node.identifier.to_string(), t);
        }
        Type::Unit
    }

    fn typeof_if(
        &mut self,
        node: &IfNode,
        env: &mut TypeEnvironment,
        dl: &mut DiagnosticsList,
    ) -> Type {
        let predicate = self.typeof_expression(&node.predicate, env, dl);
        // We do this to make the error appear on the entire predicate
        self.current_loc = node.predicate.loc();
        self.unify(&Type::Bool, &predicate, dl);

        let consequent = self.typeof_expression(&node.consequent, env, dl);

        let Some(alternate) = node.alternate.as_ref() else {
            return Type::Unit;
        };
        let alternate = self.typeof_expression(&alternate, env, dl);

        if consequent != alternate {
            return self.add_error_diagnostic(
                ErrorDiagnostic::IncompatibleTypes(IncompatibleTypesError {
                    loc: node.loc,
                    alternate,
                    consequent,
                }),
                dl,
            );
        }
        consequent
    }

    fn typeof_identifier(&mut self, node: &IdentifierNode, env: &mut TypeEnvironment) -> Type {
        match env.bindings.get(&node.name) {
            Some(t) => t.clone(),
            None => Type::Never,
        }
    }

    fn add_error_diagnostic(&mut self, d: ErrorDiagnostic, dl: &mut DiagnosticsList) -> Type {
        dl.add(Diagnostic::Error(d));
        Type::Never
    }

    fn type_error(&mut self, lhs: &Type, rhs: &Type, dl: &mut DiagnosticsList) -> Type {
        self.add_error_diagnostic(
            ErrorDiagnostic::UnassignableType(UnassignableTypeError {
                loc: self.current_loc,
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            }),
            dl,
        );
        Type::Never
    }

    fn unify(&mut self, lhs: &Type, rhs: &Type, dl: &mut DiagnosticsList) -> Type {
        if *rhs == Type::Any || *lhs == Type::Any {
            return Type::Any;
        }

        match &lhs {
            Type::Never => self.type_error(lhs, rhs, dl),

            Type::Int => match &rhs {
                Type::Int => Type::Int,
                _ => self.type_error(&lhs, rhs, dl),
            },
            Type::Bool => match &rhs {
                Type::Bool => Type::Bool,
                _ => self.type_error(lhs, rhs, dl),
            },
            Type::String => match &rhs {
                Type::String => Type::String,
                _ => self.type_error(lhs, rhs, dl),
            },
            Type::Unit => match &rhs {
                Type::Unit => Type::Unit,
                _ => self.type_error(lhs, rhs, dl),
            },
            Type::Fn(a) => match &rhs {
                Type::Fn(b) => {
                    for (a, b) in a.parameters.iter().zip(b.parameters.iter()) {
                        if self.unify(a, b, dl) == Type::Never {
                            return self.type_error(lhs, rhs, dl);
                        }
                    }

                    if self.unify(&a.return_type, &b.return_type, dl) == Type::Never {
                        return self.type_error(lhs, rhs, dl);
                    }

                    lhs.clone()
                }
                _ => self.type_error(lhs, rhs, dl),
            },
            Type::Object(a) => match &rhs {
                Type::Object(b) => {
                    for PropertyType { name, value } in &a.properties {
                        let b_prop = b.properties.iter().find(|p| p.name == *name);
                        let Some(b_prop) = b_prop else {
                            return self.type_error(lhs, rhs, dl);
                        };

                        if Type::Never == self.unify(value, &b_prop.value, dl) {
                            return self.type_error(lhs, rhs, dl);
                        };
                    }

                    lhs.clone()
                }

                _ => self.type_error(lhs, rhs, dl),
            },
            Type::Array(a) => match &rhs {
                Type::Array(b) => self.unify(&a.elements, &b.elements, dl),
                _ => self.type_error(lhs, rhs, dl),
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
        let mut dl = DiagnosticsList::new();
        ts.typeof_expression(&ast, &mut env, &mut dl);
        assert!(!dl.has_errors());
    }
}
