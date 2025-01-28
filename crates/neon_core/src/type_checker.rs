use crate::{
    diagnostic::{DiagnosticKind, DiagnosticsList},
    location::{Location, WithLocation},
    parser::{
        ArrayNode, AssignmentNode, BinaryOp, BinaryOperationNode, BlockNode, BuiltinNode, ElseNode,
        Expression, FnNode, ForLoopNode, ForLoopTarget, IdentifierNode, IfNode, IndexAccessNode,
        InvocationNode, LetBindingNode, PropertyAccessNode, StructDefinitionNode,
        StructInstantiationNode, TypeExpression,
    },
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyType {
    pub name: String,
    pub value: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
    pub name: String,
    pub properties: Vec<PropertyType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub elements: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Never,
    Int,
    Bool,
    Struct(StructType),
    Array(ArrayType),
    Fn(FnType),

    String,
    Unit,
    Any,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
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
            Type::Struct(t) => format!("{} {{..}}", t.name),
        };

        write!(f, "{}", s)
    }
}

#[derive(Clone)]
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

pub struct TypeChecker<'a> {
    current_loc: Location,
    dl: &'a mut DiagnosticsList,
}

impl TypeChecker<'_> {
    pub fn new<'a>(dl: &'a mut DiagnosticsList) -> TypeChecker<'a> {
        TypeChecker {
            current_loc: Location::beginning(),
            dl,
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
            Expression::Binary(node) => self.typeof_binary(node, env),
            Expression::Invocation(node) => self.typeof_invocation(node, env),
            Expression::Else(node) => self.typeof_else(node, env),
            Expression::Fn(node) => self.typeof_fn(node, env),
            Expression::ForLoop(node) => self.typeof_for_loop(node, env),
            Expression::Array(node) => self.typeof_array(node, env),
            Expression::PropertyAccess(node) => self.typeof_property_access(node, env),
            Expression::IndexAccess(node) => self.typeof_index_access(node, env),
            Expression::Builtin(node) => self.typeof_builtin(node, env),
            Expression::Use(node) => self.typeof_identifier(&node.identifier, env),
            Expression::StructDefinitionNode(node) => self.typeof_struct_definition(node, env),
            Expression::StructInstantiation(node) => self.typeof_struct_instantiation(node, env),

            Expression::Bool(..) => Type::Bool,
            Expression::Empty(..) => Type::Unit,
            Expression::Int(..) => Type::Int,
            Expression::String(..) => Type::String,
        }
    }

    fn typeof_struct_instantiation(
        &mut self,
        node: &StructInstantiationNode,
        env: &mut TypeEnvironment,
    ) -> Type {
        let defined_type = env.bindings.get(&node.identifier.name);

        // Symbol table should have produced a diagnostic already of we branch in the else clause
        // here
        let Some(Type::Struct(defined_type)) = defined_type else {
            return Type::Unit;
        };

        let return_type = defined_type.clone();

        for p in &node.arguments {
            let lhs = return_type
                .properties
                .iter()
                .find(|t| t.name == p.name.value);

            let Some(lhs) = lhs else {
                self.error(
                    DiagnosticKind::SuperfluousProperty {
                        key: p.name.value.clone(),
                        access_type: Type::Struct(return_type.clone()),
                    },
                    p.loc,
                );
                continue;
            };

            let rhs = self.typeof_expression(&p.value, env);

            if self.unify(&lhs.value, &rhs) == Type::Never {
                self.dl.add_error(
                    DiagnosticKind::MismatchedTypes {
                        expected: lhs.value.clone(),
                        found: rhs,
                    },
                    p.loc,
                );
            };
        }

        let property_names = node
            .arguments
            .iter()
            .map(|p| p.name.value.clone())
            .collect::<HashSet<_>>();

        for missing_prop in return_type
            .properties
            .iter()
            .filter(|t| !property_names.contains(&t.name))
        {
            self.error(
                DiagnosticKind::MissingProperty {
                    property: missing_prop.name.clone(),
                    missing_from: Type::Struct(return_type.clone()),
                },
                node.identifier.loc,
            );
        }

        Type::Struct(return_type)
    }

    fn typeof_builtin(&mut self, _node: &BuiltinNode, _env: &mut TypeEnvironment) -> Type {
        Type::Any
    }

    fn typeof_index_access(&mut self, node: &IndexAccessNode, env: &mut TypeEnvironment) -> Type {
        let typeof_index = self.typeof_expression(&node.index, env);
        self.check_type_match(&Type::Int, &typeof_index, node.index.loc());
        let typeof_indexee = self.typeof_expression(&node.indexee, env);
        let Type::Array(array_type) = typeof_indexee else {
            return self.error(
                DiagnosticKind::InvalidIndexAccess {
                    indexee_type: typeof_indexee,
                },
                node.loc,
            );
        };

        *array_type.elements
    }

    fn typeof_property_access(
        &mut self,
        node: &PropertyAccessNode,
        env: &mut TypeEnvironment,
    ) -> Type {
        let access_type = self.typeof_expression(&node.object, env);
        let Type::Struct(StructType { properties, .. }) = self.typeof_expression(&node.object, env)
        else {
            return self.error(
                DiagnosticKind::MissingPropertyAccess {
                    accessed_on: access_type,
                    property: node.identifier.name.clone(),
                },
                node.identifier.loc,
            );
        };

        let prop_type = properties.iter().find(|p| p.name == node.identifier.name);

        let Some(prop_type) = prop_type else {
            return self.error(
                DiagnosticKind::MissingPropertyAccess {
                    accessed_on: access_type,
                    property: node.identifier.name.clone(),
                },
                node.identifier.loc,
            );
        };

        prop_type.value.clone()
    }

    fn typeof_array(&mut self, node: &ArrayNode, env: &mut TypeEnvironment) -> Type {
        if node.elements.is_empty() {
            // If array is empty it can be assignable to any type
            return Type::Any;
        }

        let t = self.typeof_expression(&node.elements[0], env);
        for element in node.elements.iter().skip(1) {
            let inner_t = self.typeof_expression(element, env);
            self.current_loc = node.elements[0].loc();
            if t != inner_t {
                self.type_error(&t, &inner_t);
            }
        }

        Type::Array(ArrayType {
            elements: Box::new(t),
        })
    }

    fn typeof_else(&mut self, node: &ElseNode, env: &mut TypeEnvironment) -> Type {
        self.typeof_expression(&node.consequent, env)
    }

    fn typeof_for_loop(&mut self, node: &ForLoopNode, env: &mut TypeEnvironment) -> Type {
        let typeof_iterable = self.typeof_expression(&node.iterable, env);

        let mut env = env.clone();
        match typeof_iterable {
            Type::Struct(..) => match &node.targets {
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
                return self.error(
                    DiagnosticKind::NotIterable {
                        non_iterable: typeof_iterable,
                    },
                    node.iterable.loc(),
                )
            }
        }

        self.typeof_expression(&node.body, &mut env);
        Type::Unit
    }

    fn typeof_fn(&mut self, node: &FnNode, env: &mut TypeEnvironment) -> Type {
        let mut parameters = vec![];
        for param in &node.parameters {
            let param_type = self.typeof_type_expression(&param.type_expr, env);
            env.bindings
                .insert(param.identifier.name.clone(), param_type.clone());
            parameters.push(param_type);
        }

        let return_type = match &node.return_type {
            Some(t) => self.typeof_type_expression(&t, env),
            None => Type::Unit,
        };

        let fn_type = Type::Fn(FnType {
            parameters,
            return_type: Box::new(return_type.clone()),
        });

        let mut env = env.clone();
        env.bindings.insert(node.identifier.name.clone(), fn_type);
        let inferred_return_type = self.typeof_expression(&node.body.return_val, &mut env);
        Type::Unit
    }

    fn check_type_match(&mut self, lhs: &Type, rhs: &Type, loc: impl Into<Location>) -> Type {
        let t = self.unify(lhs, rhs);
        if t == Type::Never {
            self.dl.add_error(
                DiagnosticKind::MismatchedTypes {
                    expected: lhs.clone(),
                    found: rhs.clone(),
                },
                loc.into(),
            );
        };
        t
    }

    fn typeof_invocation(&mut self, node: &InvocationNode, env: &mut TypeEnvironment) -> Type {
        let callee = self.typeof_expression(&node.callee, env);

        let Type::Fn(fn_type) = callee else {
            return self.error(
                DiagnosticKind::ExpressionNotInvocable {
                    callee_type: callee,
                },
                node.callee.loc(),
            );
        };

        for (i, param) in fn_type.parameters.iter().enumerate() {
            let arg = node.arguments.get(i);
            let Some(arg) = arg else {
                return self.error(
                    DiagnosticKind::InsufficientArguments {
                        expected: fn_type.parameters.len(),
                        got: node.arguments.len(),
                    },
                    node.loc,
                );
            };
            let arg_type = self.typeof_expression(arg, env);
            self.check_type_match(param, &arg_type, arg.loc());
        }
        *fn_type.return_type
    }

    fn typeof_binary(&mut self, node: &BinaryOperationNode, env: &mut TypeEnvironment) -> Type {
        let lhs = self.typeof_expression(&node.left, env);
        let rhs = self.typeof_expression(&node.right, env);
        let unification = self.check_type_match(&lhs, &rhs, node.left.loc());

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

    fn typeof_type_expression(&mut self, node: &TypeExpression, env: &mut TypeEnvironment) -> Type {
        match node {
            TypeExpression::Int(..) => Type::Int,
            TypeExpression::String(..) => Type::String,
            TypeExpression::Bool(..) => Type::Bool,
            TypeExpression::Unit(..) => Type::Unit,
            TypeExpression::Fn(node) => Type::Fn(FnType {
                return_type: Box::new(self.typeof_type_expression(&node.return_type, env)),
                parameters: node
                    .parameters
                    .iter()
                    .map(|t| self.typeof_type_expression(t, env))
                    .collect(),
            }),
            TypeExpression::Array(node) => Type::Array(ArrayType {
                elements: Box::new(self.typeof_type_expression(&node.elements, env)),
            }),
            TypeExpression::Identifier(node) => match env.bindings.get(&node.name) {
                Some(t) => t.clone(),
                None => self.error(
                    DiagnosticKind::UndefinedType {
                        name: node.name.clone(),
                    },
                    node.loc,
                ),
            },
        }
    }

    fn typeof_let_binding(&mut self, node: &LetBindingNode, env: &mut TypeEnvironment) -> Type {
        let rhs = if let Some(right) = &node.right.as_ref() {
            self.typeof_expression(right, env)
        } else {
            Type::Unit
        };

        let t = if let Some(type_exp) = &node.binding.type_expr {
            let lhs = self.typeof_type_expression(&type_exp, env);
            let loc = node
                .right
                .clone()
                .map(|r| r.loc())
                .unwrap_or(type_exp.loc());
            self.check_type_match(&lhs, &rhs, loc);
            lhs
        } else {
            rhs
        };

        env.bindings.insert(node.binding.identifier.name.clone(), t);
        Type::Unit
    }

    fn typeof_struct_definition(
        &mut self,
        node: &StructDefinitionNode,
        env: &mut TypeEnvironment,
    ) -> Type {
        let properties = node
            .properties
            .iter()
            .map(|n| PropertyType {
                name: n.name.value.clone(),
                value: self.typeof_type_expression(&n.type_expr, env),
            })
            .collect::<Vec<_>>();

        env.bindings.insert(
            node.identifier.name.clone(),
            Type::Struct(StructType {
                properties,
                name: node.identifier.name.clone(),
            }),
        );
        Type::Unit
    }

    fn typeof_assignment(&mut self, node: &AssignmentNode, env: &mut TypeEnvironment) -> Type {
        let mut lhs = self.typeof_identifier(&node.identifier, env);
        let rhs = self.typeof_expression(&node.right, env);

        // If the lhs is unit we give it the type of the rhs
        if lhs == Type::Unit {
            lhs = rhs.clone();
        };

        let t = self.check_type_match(&lhs, &rhs, node.identifier.loc);
        if t != Type::Never {
            env.bindings.insert(node.identifier.to_string(), t);
        }
        Type::Unit
    }

    fn typeof_if(&mut self, node: &IfNode, env: &mut TypeEnvironment) -> Type {
        let predicate = self.typeof_expression(&node.predicate, env);
        self.check_type_match(&Type::Bool, &predicate, node.predicate.loc());

        let consequent = self.typeof_expression(&node.consequent, env);

        let Some(alternate) = node.alternate.as_ref() else {
            return Type::Unit;
        };
        let alternate = self.typeof_expression(&alternate, env);

        let result = self.unify(&consequent, &alternate);
        if result == Type::Never {
            return self.error(
                DiagnosticKind::IncompatibleTypes {
                    alternate,
                    consequent,
                },
                node.loc,
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

    fn unify(&mut self, lhs: &Type, rhs: &Type) -> Type {
        if *rhs == Type::Any || *lhs == Type::Any {
            return Type::Any;
        }

        match &lhs {
            Type::Never => self.type_error(lhs, rhs),

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
            Type::Fn(a) => match &rhs {
                Type::Fn(b) => {
                    for (a, b) in a.parameters.iter().zip(b.parameters.iter()) {
                        if self.unify(a, b) == Type::Never {
                            return self.type_error(lhs, rhs);
                        }
                    }

                    if self.unify(&a.return_type, &b.return_type) == Type::Never {
                        return self.type_error(lhs, rhs);
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

    fn error(&mut self, d: DiagnosticKind, loc: Location) -> Type {
        self.dl.add_error(d, loc);
        Type::Never
    }

    fn type_error(&mut self, lhs: &Type, rhs: &Type) -> Type {
        self.dl.add_error(
            DiagnosticKind::UnassignableType {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            },
            self.current_loc,
        );
        Type::Never
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
        let mut dl = DiagnosticsList::new();
        let mut ts = TypeChecker::new(&mut dl);
        let mut env = TypeEnvironment {
            bindings: HashMap::new(),
        };
        ts.typeof_expression(&ast, &mut env);
        assert!(!dl.has_errors());
    }
}
