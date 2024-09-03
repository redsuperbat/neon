use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use crate::{
    lexer::Pos,
    parser::{BinaryOp, BuiltinExpressionKind, Expression, ExpressionKind},
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int { value: i64 },
    String { value: String },
    Bool { value: bool },
    Fn { function: ExpressionKind },
    Array { elements: Vec<Value> },
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int { value } => write!(f, "{}", value),
            Value::Bool { value } => write!(f, "{}", value),
            Value::Fn { function } => {
                let ExpressionKind::Fn { name, .. } = function else {
                    panic!("invalid type")
                };
                write!(f, "Function: {}", name)
            }
            Value::Unit => write!(f, "Unit"),
            Value::String { value } => write!(f, "\"{}\"", value),
            Value::Array { elements } => {
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
    pub start: Pos,
    pub end: Pos,
}

impl RuntimeError {
    fn type_error(start: &Pos, end: &Pos) -> RuntimeError {
        RuntimeError {
            kind: RuntimeErrorKind::TypeError,
            end: end.clone(),
            start: start.clone(),
        }
    }
    fn illegal_invocation(start: &Pos, end: &Pos) -> RuntimeError {
        RuntimeError {
            kind: RuntimeErrorKind::IllegalInvocation,
            end: end.clone(),
            start: start.clone(),
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
        let func = Value::Fn {
            function: ExpressionKind::Fn {
                name: name.clone(),
                parameters: arguments.clone(),
                body: Expression::kind(ExpressionKind::Builtin {
                    kind: kind.clone(),
                    arguments,
                })
                .boxed(),
            },
        };
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
        let Expression { start, end, kind } = expression;
        match kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body: block,
            } => self.evaluate_fn(name, parameters, block, ctx),
            ExpressionKind::Identifier { name } => self.evaluate_identifier(name, start, end, ctx),
            ExpressionKind::Invocation { callee, arguments } => {
                self.evaluate_invocation(callee, arguments, ctx)
            }
            ExpressionKind::LetBinding { name, right } => self.evaluate_let(name, right, ctx),
            ExpressionKind::Int { value } => self.evaluate_int(value),
            ExpressionKind::Block { body, return_val } => {
                self.evaluate_block(body, return_val, ctx)
            }
            ExpressionKind::If {
                predicate,
                consequent,
                alternate,
            } => self.evaluate_if(predicate, consequent, alternate, ctx),
            ExpressionKind::Else { consequent } => self.evaluate_expression(consequent, ctx),
            ExpressionKind::Bool { value } => Ok(Value::Bool { value: *value }),
            ExpressionKind::String { value } => Ok(Value::String {
                value: value.clone(),
            }),
            ExpressionKind::Binary {
                operation,
                left,
                right,
            } => match operation {
                BinaryOp::Add => self.evaluate_binary_add(left, right, ctx),
                BinaryOp::Ne => self.evaluate_binary_ne(left, right, ctx),
                BinaryOp::Eq => self.evaluate_binary_eq(left, right, ctx),
                BinaryOp::Sub => self.evaluate_binary_subtract(left, right, ctx),
                BinaryOp::Lt => self.evaluate_binary_lt(left, right, ctx),
                BinaryOp::Gt => self.evaluate_binary_gt(left, right, ctx),
                BinaryOp::Mod => self.evaluate_modulus(left, right, ctx),
                BinaryOp::And => self.evaluate_and(left, right, ctx),
                BinaryOp::Or => self.evaluate_or(left, right, ctx),
            },
            ExpressionKind::Empty => Ok(Value::Unit),
            ExpressionKind::Builtin { arguments, kind } => {
                self.evaluate_internal(kind, arguments, ctx)
            }
            ExpressionKind::Array { elements } => self.evaluate_array(elements, ctx),
            ExpressionKind::IndexAccess { indexee, index } => {
                self.evaluate_index_access(indexee, index, ctx)
            }
        }
    }

    fn evaluate_index_access(
        &self,
        indexee: &Expression,
        index: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let array = self.evaluate_expression(indexee, ctx)?;
        let Value::Array { elements } = array else {
            return Err(RuntimeError::type_error(&indexee.start, &indexee.end));
        };
        let index_value = self.evaluate_expression(index, ctx)?;
        let Value::Int { value: i } = index_value else {
            return Err(RuntimeError::type_error(&index.start, &index.end));
        };
        let value = elements.get(i as usize);
        match value {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError {
                start: index.start,
                end: index.end,
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

        Ok(Value::Array { elements })
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
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l - r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };

        Ok(Value::Int { value: result })
    }

    fn evaluate_binary_ne(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l != r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l != r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            Value::String { value: l } => match right {
                Value::String { value: r } => l != r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_binary_eq(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l == r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l == r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            Value::String { value: l } => match right {
                Value::String { value: r } => l == r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_or(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l || r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_and(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l && r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_modulus(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l % r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Int { value: result })
    }

    fn evaluate_binary_gt(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l > r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_binary_lt(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l < r,
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_if(
        &self,
        predicate: &Expression,
        consequent: &Expression,
        alternate: &Option<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &predicate.start;
        let end = &predicate.end;
        let value = self.evaluate_expression(predicate, ctx)?;
        let Value::Bool { value } = value else {
            return Err(RuntimeError::type_error(start, end));
        };

        if value {
            return self.evaluate_expression(consequent, ctx);
        };

        if let Some(alternate) = alternate {
            return self.evaluate_expression(alternate, ctx);
        };

        Ok(Value::Unit)
    }

    fn evaluate_block(
        &self,
        body: &Vec<Expression>,
        return_val: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        for exp in body {
            self.evaluate_expression(exp, ctx)?;
        }
        self.evaluate_expression(return_val, ctx)
    }

    fn evaluate_fn(
        &self,
        name: &str,
        parameters: &Vec<String>,
        block: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = Value::Fn {
            function: ExpressionKind::Fn {
                name: name.to_string(),
                parameters: parameters.clone(),
                body: Box::new(block.clone()),
            },
        };
        ctx.bindings.insert(name.to_string(), value.clone());
        Ok(value)
    }

    fn evaluate_invocation(
        &self,
        callee: &Expression,
        arguments: &Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &callee.start;
        let end = &callee.end;
        let value = self.evaluate_expression(callee, ctx)?;

        let Value::Fn { function } = value else {
            return Err(RuntimeError::illegal_invocation(start, end));
        };

        let ExpressionKind::Fn {
            parameters,
            body,
            name,
            ..
        } = function
        else {
            return Err(RuntimeError::type_error(start, end));
        };

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
        start: &Pos,
        end: &Pos,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let declaration = ctx.symbol_table.get_declaration(name).ok_or(RuntimeError {
            end: *end,
            start: *start,
            kind: RuntimeErrorKind::UndefinedReference,
        })?;

        let value = ctx.bindings.get(declaration).ok_or(RuntimeError {
            kind: RuntimeErrorKind::UninitializedVariable,
            end: *end,
            start: *start,
        })?;

        return Ok(value.clone());
    }

    fn evaluate_let(
        &self,
        name: &str,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate_expression(right, ctx)?;
        ctx.bindings.insert(name.to_string(), value);
        Ok(Value::Unit)
    }

    fn evaluate_int(&self, value: &i64) -> Result<Value, RuntimeError> {
        Ok(Value::Int { value: *value })
    }

    fn evaluate_binary_add(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let start = &left.start;
        let end = &right.end;
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => Value::Int { value: l + r },
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            Value::String { value: l } => match right {
                Value::String { value: r } => Value::String { value: l + &r },
                _ => return Err(RuntimeError::type_error(start, end)),
            },
            _ => return Err(RuntimeError::type_error(start, end)),
        };

        Ok(result)
    }
}
