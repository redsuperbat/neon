use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use crate::{
    location::Location,
    parser::{
        Binary, BinaryOp, Block, Builtin as BuiltinExp, BuiltinExpressionKind, Expression,
        ExpressionKind, Fn, ForLoop, If, IndexAccess, Invocation, LetBinding,
    },
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    String(String),
    Bool(bool),
    Fn(Fn),
    Array(Vec<Value>),
    Unit,
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
            Value::String(value) => write!(f, "{}", value),
            Value::Array(elements) => {
                let mut str = String::from("[");

                for (i, element) in elements.iter().enumerate() {
                    if i == 0 {
                        str += &format!("{}", element);
                    } else {
                        str += &format!(", {}", element);
                    }
                }

                str += "]";

                write!(f, "{}", str)
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
}

impl ToString for RuntimeErrorKind {
    fn to_string(&self) -> String {
        match self {
            RuntimeErrorKind::TypeError => "Type error".to_string(),
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
    pub symbol_table: SymbolTable,
    pub bindings: HashMap<String, Value>,
    pub call_stack: Vec<String>,
}

impl EvaluationContext {
    pub fn new(symbol_table: SymbolTable) -> EvaluationContext {
        EvaluationContext {
            symbol_table,
            bindings: HashMap::new(),
            call_stack: vec![],
        }
    }
    pub fn register_bultin(&mut self, kind: &BuiltinExpressionKind) {
        let arguments: Vec<String> = (0..=100).map(|n| n.to_string() + "arg").collect();
        let name = kind.name();
        let function_expression = Fn {
            name: name.clone(),
            parameters: arguments.clone(),
            body: ExpressionKind::Builtin(BuiltinExp {
                kind: kind.clone(),
                arguments,
            })
            .into_exp(Location::beginning())
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
        let Expression { loc, kind } = expression;
        match kind {
            ExpressionKind::Fn(f) => self.evaluate_fn(f, ctx),
            ExpressionKind::Identifier(name) => self.evaluate_identifier(name, loc, ctx),
            ExpressionKind::Invocation(invocation) => self.evaluate_invocation(invocation, ctx),
            ExpressionKind::LetBinding(let_bind) => self.evaluate_let(let_bind, ctx),
            ExpressionKind::Int(value) => self.evaluate_int(value),
            ExpressionKind::Block(block) => self.evaluate_block(block, ctx),
            ExpressionKind::If(exp) => self.evaluate_if(exp, ctx),
            ExpressionKind::Else(consequent) => self.evaluate_expression(consequent, ctx),
            ExpressionKind::Bool(value) => Ok(Value::Bool(*value)),
            ExpressionKind::String(value) => Ok(Value::String(value.clone())),
            ExpressionKind::Binary(binary) => self.evaluate_binary(binary, ctx),
            ExpressionKind::Empty => Ok(Value::Unit),
            ExpressionKind::Builtin(BuiltinExp { kind, arguments }) => {
                self.evaluate_internal(kind, arguments, ctx)
            }
            ExpressionKind::Array(elements) => self.evaluate_array(elements, ctx),
            ExpressionKind::IndexAccess(access) => self.evaluate_index_access(access, ctx),
            ExpressionKind::ForLoop(for_loop) => self.evaluate_for_loop(for_loop, loc, ctx),
            ExpressionKind::PropertyAccess { .. } => todo!(),
        }
    }

    fn evaluate_for_loop(
        &self,
        for_loop: &ForLoop,
        loc: &Location,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let ForLoop {
            target,
            iterable,
            body,
        } = for_loop;

        let Ok(Value::Array(elements)) = self.evaluate_expression(iterable, ctx) else {
            return Err(RuntimeError::type_error(loc));
        };

        let ExpressionKind::Identifier(name) = target.kind.to_owned() else {
            return Err(RuntimeError::type_error(loc));
        };

        for el in elements {
            let mut loop_ctx = ctx.clone();
            loop_ctx.bindings.insert(name.clone(), el);
            self.evaluate_expression(&body, &mut loop_ctx)?;
        }

        Ok(Value::Unit)
    }

    fn evaluate_binary(
        &self,
        binary: &Binary,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let Binary {
            left,
            right,
            operation,
        } = binary;
        let start = &left.loc.start;
        let end = &right.loc.end;
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
        access: &IndexAccess,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let IndexAccess { indexee, index } = access;
        let array = self.evaluate_expression(indexee, ctx)?;
        let Value::Array(elements) = array else {
            return Err(RuntimeError::type_error(indexee.loc));
        };
        let index_value = self.evaluate_expression(index, ctx)?;
        let Value::Int(i) = index_value else {
            return Err(RuntimeError::type_error(indexee.loc));
        };
        let value = elements.get(i as usize);
        match value {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError {
                loc: index.loc,
                kind: RuntimeErrorKind::IndexOutOfBounds { index: i as usize },
            }),
        }
    }

    fn evaluate_array(
        &self,
        elements: &Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let elements = elements
            .iter()
            .map(|e| self.evaluate_expression(e, ctx))
            .collect::<Result<Vec<Value>, RuntimeError>>()?;

        Ok(Value::Array(elements))
    }

    fn evaluate_internal(
        &self,
        kind: &BuiltinExpressionKind,
        arguments: &Vec<String>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
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

    fn evaluate_if(&self, exp: &If, ctx: &mut EvaluationContext) -> Result<Value, RuntimeError> {
        let If {
            predicate,
            alternate,
            consequent,
        } = exp;
        let value = self.evaluate_expression(predicate, ctx)?;
        let Value::Bool(value) = value else {
            return Err(RuntimeError::type_error(&predicate.loc));
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
        block: &Block,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let Block { body, return_val } = block;
        for exp in body {
            self.evaluate_expression(exp, ctx)?;
        }
        self.evaluate_expression(return_val, ctx)
    }

    fn evaluate_fn(&self, f: &Fn, ctx: &mut EvaluationContext) -> Result<Value, RuntimeError> {
        let value = Value::Fn(f.clone());
        ctx.bindings.insert(f.name.to_string(), value.clone());
        Ok(value)
    }

    fn evaluate_invocation(
        &self,
        invocation: &Invocation,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let Invocation { arguments, callee } = invocation;
        let value = self.evaluate_expression(callee, ctx)?;

        let Value::Fn(function) = value else {
            return Err(RuntimeError::illegal_invocation(&callee.loc));
        };

        let Fn {
            name,
            parameters,
            body,
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
        name: &str,
        loc: &Location,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = ctx.bindings.get(name).ok_or(RuntimeError {
            kind: RuntimeErrorKind::UninitializedVariable,
            loc: *loc,
        })?;

        return Ok(value.clone());
    }

    fn evaluate_let(
        &self,
        let_bind: &LetBinding,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let LetBinding { name, right } = let_bind;
        let value = self.evaluate_expression(right, ctx)?;
        ctx.bindings.insert(name.to_string(), value);
        Ok(Value::Unit)
    }

    fn evaluate_int(&self, value: &i64) -> Result<Value, RuntimeError> {
        Ok(Value::Int(*value))
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
