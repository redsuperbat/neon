use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use crate::{
    location::Location,
    parser::{
        ArrayNode, AssignmentNode, BinaryOp, BinaryOperationNode, BlockNode, BuiltinExpressionKind,
        BuiltinNode, Expression, FnNode, ForLoopNode, ForLoopTarget, IdentifierNode, IfNode,
        IndexAccessNode, IntNode, InvocationNode, LetBindingNode, ObjectNode, PropertyAccessNode,
    },
};

#[derive(Debug, Clone)]
pub struct Object(pub HashMap<String, Value>);

impl Object {
    fn single_line_readable(&self) -> String {
        let mut result = String::from("{ ");

        let mut first = true;
        for (key, value) in self.0.iter() {
            if first {
                first = false;
            } else {
                result += ", "
            }

            let value_str = if let Value::Object(obj) = value {
                obj.to_readable(0)
            } else {
                format!("{}", value)
            };

            result += &format!("{key}: {}", value_str);
        }
        result += " }";
        result
    }

    fn multi_line_readable(&self, indent: usize) -> String {
        let mut result = String::from("{\n");
        let indentation = " ".repeat(indent);

        let mut first = true;
        for (key, value) in self.0.iter() {
            if first {
                first = false;
            } else {
                result += ",\n"
            }

            let value_str = match value {
                Value::Array(arr) => arr.to_readable(indent + 1),
                Value::Object(obj) => obj.to_readable(indent + 1),
                _ => format!("{}", value),
            };

            result += &format!("{}{key}: {}", indentation, value_str);
        }
        let indentation = " ".repeat(indent - 1);
        result += &format!("\n{indentation}}}");
        result
    }

    pub fn to_readable(&self, indent: usize) -> String {
        if self.0.len() == 0 {
            return "{}".to_string();
        }
        if self.0.len() < 3 {
            return self.single_line_readable();
        }
        return self.multi_line_readable(indent);
    }
}

#[derive(Debug, Clone)]
pub struct Array(pub Vec<Value>);
impl Array {
    fn single_line_readable(&self) -> String {
        let arr_str = self
            .0
            .iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(",");

        format!("[{arr_str}]")
    }

    fn multi_line_readable(&self, indent: usize) -> String {
        let arr_str = self
            .0
            .iter()
            .map(|v| format!("{}{}", " ".repeat(indent), v))
            .collect::<Vec<_>>()
            .join(",\n");

        let indentation = " ".repeat(indent - 1);

        format!("[\n{arr_str}\n{indentation}]")
    }

    pub fn to_readable(&self, indent: usize) -> String {
        if self.0.len() < 6 {
            return self.single_line_readable();
        }
        return self.multi_line_readable(indent);
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
    Fn(FnNode),
    Array(Array),
    Unit,
    Object(Object),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Fn(function) => {
                write!(f, "Function: {}", function.name)
            }
            Value::Unit => write!(f, "Unit"),
            Value::String(value) => write!(f, "\"{}\"", value),
            Value::Object(obj) => {
                write!(f, "{}", obj.to_readable(1))
            }
            Value::Array(arr) => {
                write!(f, "{}", arr.to_readable(1))
            }
        }
    }
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError,
    UndefinedReference,
    UninitializedVariable,
    IllegalInvocation,
    IndexOutOfBounds { index: usize },
    InvalidPropertyAccess { name: String, value: Value },
}

impl ToString for RuntimeErrorKind {
    fn to_string(&self) -> String {
        match self {
            RuntimeErrorKind::TypeError => "Type error".to_string(),
            RuntimeErrorKind::InvalidPropertyAccess { name, value } => {
                format!(
                    "Cannot access property of {}, reading property \"{name}\"",
                    value
                )
            }
            RuntimeErrorKind::UndefinedReference => "Reference is undefined".to_string(),
            RuntimeErrorKind::UninitializedVariable => "Variable is uninitialized".to_string(),
            RuntimeErrorKind::IllegalInvocation => "Callee was not a valid function".to_string(),
            RuntimeErrorKind::IndexOutOfBounds { index } => {
                format!("Index '{}' out of bounds", index)
            }
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub loc: Location,
}

impl RuntimeError {
    fn type_error<T: Into<Location>>(loc: T) -> RuntimeError {
        RuntimeError {
            kind: RuntimeErrorKind::TypeError,
            loc: loc.into(),
        }
    }
    fn illegal_invocation(loc: &Location) -> RuntimeError {
        RuntimeError {
            kind: RuntimeErrorKind::IllegalInvocation,
            loc: *loc,
        }
    }
}

pub trait Builtin {
    fn exec(&self, values: Vec<&Value>) -> Result<Value, RuntimeError>;
}

pub struct Interpreter {
    builtins: HashMap<BuiltinExpressionKind, Box<dyn Builtin>>,
}

#[derive(Debug, Clone)]
pub struct EvaluationContext {
    pub bindings: HashMap<String, Value>,
    pub call_stack: Vec<String>,
}

impl EvaluationContext {
    pub fn new() -> EvaluationContext {
        EvaluationContext {
            bindings: HashMap::new(),
            call_stack: vec![],
        }
    }

    pub fn register_bultin(&mut self, kind: &BuiltinExpressionKind) {
        let arguments: Vec<String> = (0..=100).map(|n| n.to_string() + "arg").collect();
        let name = kind.name();
        let function_expression = FnNode {
            loc: Location::beginning(),
            name: name.clone(),
            parameters: arguments.clone(),
            body: Expression::Builtin(BuiltinNode {
                loc: Location::beginning(),
                kind: kind.clone(),
                arguments,
            })
            .boxed(),
        };

        let func = Value::Fn(function_expression);
        self.bindings.insert(name, func);
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            builtins: HashMap::new(),
        }
    }

    pub fn register_bultin(
        &mut self,
        kind: &BuiltinExpressionKind,
        builtin: Box<dyn Builtin>,
    ) -> () {
        self.builtins.insert(kind.clone(), builtin);
    }

    pub fn evaluate_expression(
        &self,
        expression: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        match expression {
            Expression::Fn(node) => self.evaluate_fn(node, ctx),
            Expression::Identifier(node) => self.evaluate_identifier(node, ctx),
            Expression::Invocation(node) => self.evaluate_invocation(node, ctx),
            Expression::LetBinding(node) => self.evaluate_let(node, ctx),
            Expression::Int(node) => self.evaluate_int(node),
            Expression::Block(node) => self.evaluate_block(node, ctx),
            Expression::If(node) => self.evaluate_if(node, ctx),
            Expression::Else(node) => self.evaluate_expression(&node.consequent, ctx),
            Expression::Binary(node) => self.evaluate_binary(node, ctx),
            Expression::Array(node) => self.evaluate_array(node, ctx),
            Expression::IndexAccess(node) => self.evaluate_index_access(node, ctx),
            Expression::ForLoop(node) => self.evaluate_for_loop(node, ctx),
            Expression::PropertyAccess(node) => self.evaluate_property_access(node, ctx),
            Expression::Object(node) => self.evaluate_object(node, ctx),
            Expression::Builtin(node) => self.evaluate_internal(node, ctx),
            Expression::Assignment(node) => self.evaluate_assignment(node, ctx),

            Expression::String(node) => Ok(Value::String(node.value.clone())),
            Expression::Bool(node) => Ok(Value::Bool(node.value)),
            Expression::Empty(..) => Ok(Value::Unit),
        }
    }

    fn evaluate_assignment(
        &self,
        node: &AssignmentNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate_expression(&node.right, ctx)?;
        ctx.bindings.insert(node.identifier.name.clone(), value);
        Ok(Value::Unit)
    }

    fn evaluate_property_access(
        &self,
        node: &PropertyAccessNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate_expression(node.object.as_ref(), ctx)?;

        let Value::Object(obj) = value else {
            return Err(RuntimeError {
                loc: node.loc,
                kind: RuntimeErrorKind::InvalidPropertyAccess {
                    name: node.property_name.name.to_string(),
                    value,
                },
            });
        };

        match obj.0.get(&node.property_name.name) {
            Some(v) => Ok(v.clone()),
            None => Ok(Value::Unit),
        }
    }

    fn evaluate_object(
        &self,
        obj: &ObjectNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let mut map = HashMap::new();

        for p in &obj.properties {
            let value = self.evaluate_expression(p.value.as_ref(), ctx)?;
            map.insert(p.identifier.name.clone(), value);
        }

        Ok(Value::Object(Object(map)))
    }

    fn evaluate_for_loop(
        &self,
        for_loop: &ForLoopNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let ForLoopNode {
            targets,
            iterable,
            body,
            loc,
        } = for_loop;

        let mut loop_ctx = ctx.clone();

        match targets {
            ForLoopTarget::Single(IdentifierNode { name, .. }) => {
                match self.evaluate_expression(iterable, ctx)? {
                    Value::String(string) => {
                        for s in string.chars() {
                            loop_ctx
                                .bindings
                                .insert(name.clone(), Value::String(s.to_string()));
                            self.evaluate_expression(&body, &mut loop_ctx)?;
                        }
                    }
                    Value::Array(Array(elements)) => {
                        for el in elements {
                            loop_ctx.bindings.insert(name.clone(), el);
                            self.evaluate_expression(&body, &mut loop_ctx)?;
                        }
                    }
                    Value::Object(obj) => {
                        for (_, value) in obj.0 {
                            loop_ctx.bindings.insert(name.clone(), value);
                            self.evaluate_expression(&body, &mut loop_ctx)?;
                        }
                    }
                    _ => return Err(RuntimeError::type_error(loc)),
                }
            }
            ForLoopTarget::Tuple(
                IdentifierNode { name: first, .. },
                IdentifierNode { name: second, .. },
            ) => match self.evaluate_expression(iterable, ctx)? {
                Value::String(string) => {
                    for (i, s) in string.chars().enumerate() {
                        loop_ctx
                            .bindings
                            .insert(first.clone(), Value::Int(i as i64));
                        loop_ctx
                            .bindings
                            .insert(second.clone(), Value::String(s.to_string()));
                        self.evaluate_expression(&body, &mut loop_ctx)?;
                    }
                }
                Value::Array(elements) => {
                    for (i, el) in elements.0.iter().enumerate() {
                        loop_ctx
                            .bindings
                            .insert(first.clone(), Value::Int(i as i64));
                        loop_ctx.bindings.insert(second.clone(), el.clone());
                        self.evaluate_expression(&body, &mut loop_ctx)?;
                    }
                }
                Value::Object(obj) => {
                    for (key, value) in obj.0 {
                        loop_ctx.bindings.insert(first.clone(), Value::String(key));
                        loop_ctx.bindings.insert(second.clone(), value.clone());
                        self.evaluate_expression(&body, &mut loop_ctx)?;
                    }
                }
                _ => return Err(RuntimeError::type_error(loc)),
            },
        }

        Ok(Value::Unit)
    }

    fn evaluate_binary(
        &self,
        binary: &BinaryOperationNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let BinaryOperationNode {
            left,
            right,
            operation,
            ..
        } = binary;
        let start = &left.loc().start;
        let end = &right.loc().end;
        let loc = Location::new(start, end);
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        match operation {
            BinaryOp::Add => self.evaluate_binary_add(left, right),
            BinaryOp::Ne => self.evaluate_binary_ne(left, right),
            BinaryOp::Eq => self.evaluate_binary_eq(left, right),
            BinaryOp::Sub => self.evaluate_binary_subtract(left, right),
            BinaryOp::Lt => self.evaluate_binary_lt(left, right),
            BinaryOp::Gt => self.evaluate_binary_gt(left, right),
            BinaryOp::Mod => self.evaluate_modulus(left, right),
            BinaryOp::And => self.evaluate_and(left, right),
            BinaryOp::Or => self.evaluate_or(left, right),
        }
        .map_err(|kind| RuntimeError { kind, loc })
    }

    fn evaluate_index_access(
        &self,
        access: &IndexAccessNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let IndexAccessNode { indexee, index, .. } = access;
        let array = self.evaluate_expression(indexee, ctx)?;
        let Value::Array(elements) = array else {
            return Err(RuntimeError::type_error(indexee.loc()));
        };
        let index_value = self.evaluate_expression(index, ctx)?;
        let Value::Int(i) = index_value else {
            return Err(RuntimeError::type_error(indexee.loc()));
        };
        let value = elements.0.get(i as usize);
        match value {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError {
                loc: index.loc(),
                kind: RuntimeErrorKind::IndexOutOfBounds { index: i as usize },
            }),
        }
    }

    fn evaluate_array(
        &self,
        node: &ArrayNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let elements = node
            .elements
            .iter()
            .map(|e| self.evaluate_expression(e, ctx))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        Ok(Value::Array(Array(elements)))
    }

    fn evaluate_internal(
        &self,
        node: &BuiltinNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let BuiltinNode {
            arguments, kind, ..
        } = node;
        let values = arguments
            .iter()
            .map(|a| ctx.bindings.get(a))
            .filter_map(|a| a)
            .collect::<Vec<&Value>>();
        let builtin = self.builtins.get(kind).expect("Internal neon error");
        builtin.exec(values)
    }

    fn evaluate_binary_subtract(
        &self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l - r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };

        Ok(Value::Int(result))
    }

    fn evaluate_binary_ne(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l != r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            Value::Bool(l) => match right {
                Value::Bool(r) => l != r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            Value::String(l) => match right {
                Value::String(r) => l != r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_binary_eq(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l == r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            Value::Bool(l) => match right {
                Value::Bool(r) => l == r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            Value::String(l) => match right {
                Value::String(r) => l == r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_or(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Bool(l) => match right {
                Value::Bool(r) => l || r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_and(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Bool(l) => match right {
                Value::Bool(r) => l && r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_modulus(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l % r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Int(result))
    }

    fn evaluate_binary_gt(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l > r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_binary_lt(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => l < r,
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };
        Ok(Value::Bool(result))
    }

    fn evaluate_if(
        &self,
        exp: &IfNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let IfNode {
            predicate,
            alternate,
            consequent,
            ..
        } = exp;
        let value = self.evaluate_expression(predicate, ctx)?;
        let Value::Bool(value) = value else {
            return Err(RuntimeError::type_error(&predicate.loc()));
        };

        if value {
            return self.evaluate_expression(consequent, ctx);
        };

        if let Some(alternate) = alternate.as_ref() {
            return self.evaluate_expression(alternate, ctx);
        };

        Ok(Value::Unit)
    }

    fn evaluate_block(
        &self,
        block: &BlockNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let BlockNode {
            body, return_val, ..
        } = block;
        for exp in body {
            self.evaluate_expression(exp, ctx)?;
        }
        self.evaluate_expression(return_val, ctx)
    }

    fn evaluate_fn(&self, f: &FnNode, ctx: &mut EvaluationContext) -> Result<Value, RuntimeError> {
        let value = Value::Fn(f.clone());
        ctx.bindings.insert(f.name.to_string(), value.clone());
        Ok(value)
    }

    fn evaluate_invocation(
        &self,
        invocation: &InvocationNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let InvocationNode {
            arguments, callee, ..
        } = invocation;
        let value = self.evaluate_expression(callee, ctx)?;

        let Value::Fn(function) = value else {
            return Err(RuntimeError::illegal_invocation(&callee.loc()));
        };

        let FnNode {
            name,
            parameters,
            body,
            ..
        } = function;

        let value_args = arguments
            .iter()
            .map(|a| self.evaluate_expression(a, ctx))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        let mut func_ctx = ctx.clone();
        func_ctx.call_stack.push(name);

        parameters
            .iter()
            .zip(value_args.iter())
            .for_each(|(param, argument)| {
                func_ctx.bindings.insert(param.clone(), argument.clone());
            });

        let result = self.evaluate_expression(&body, &mut func_ctx);
        func_ctx.call_stack.pop();
        result
    }

    fn evaluate_identifier(
        &self,
        id: &IdentifierNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = ctx.bindings.get(&id.name).ok_or(RuntimeError {
            kind: RuntimeErrorKind::UninitializedVariable,
            loc: id.loc,
        })?;

        return Ok(value.clone());
    }

    fn evaluate_let(
        &self,
        let_bind: &LetBindingNode,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let LetBindingNode { name, right, .. } = let_bind;
        let value = self.evaluate_expression(right, ctx)?;
        ctx.bindings.insert(name.to_string(), value);
        Ok(Value::Unit)
    }

    fn evaluate_int(&self, node: &IntNode) -> Result<Value, RuntimeError> {
        Ok(Value::Int(node.value))
    }

    fn evaluate_binary_add(&self, left: Value, right: Value) -> Result<Value, RuntimeErrorKind> {
        let result = match left {
            Value::Int(l) => match right {
                Value::Int(r) => Value::Int(l + r),
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            Value::String(l) => match right {
                Value::String(r) => Value::String(l.to_owned() + &r),
                _ => return Err(RuntimeErrorKind::TypeError),
            },
            _ => return Err(RuntimeErrorKind::TypeError),
        };

        Ok(result)
    }
}
