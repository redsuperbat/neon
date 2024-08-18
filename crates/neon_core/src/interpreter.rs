use core::panic;
use std::{collections::HashMap, fmt::Display};

use crate::{
    lexer::Pos,
    parser::{Expression, ExpressionKind},
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int { value: i64 },
    String { value: String },
    Bool { value: bool },
    Fn { function: ExpressionKind },
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
            Value::String { value } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError,
    UndefinedReference,
    UninitializedVariable,
    IllegalInvocation,
}

impl ToString for RuntimeErrorKind {
    fn to_string(&self) -> String {
        format!("{:?}", self)
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

pub struct Interpreter {}

#[derive(Debug, Clone)]
pub struct EvaluationContext {
    pub symbol_table: SymbolTable,
    pub bindings: HashMap<String, Value>,
    pub call_stack: Vec<String>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate_expression(
        &self,
        expression: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        match &expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body: block,
            } => self.evaluate_fn(name, parameters, block, ctx),
            ExpressionKind::Identifier { name } => {
                self.evaluate_identifier(name, &expression.start, &expression.end, ctx)
            }
            ExpressionKind::Invocation { callee, arguments } => {
                self.evaluate_invocation(callee, arguments, ctx)
            }
            ExpressionKind::LetBinding { name, right } => self.evaluate_let(name, right, ctx),
            ExpressionKind::Int { value } => self.evaluate_int(value),
            ExpressionKind::Add { left, right } => self.evaluate_binary_add(left, right, ctx),
            ExpressionKind::Block { body, return_val } => {
                self.evaluate_block(body, return_val, ctx)
            }
            ExpressionKind::If {
                predicate,
                consequent,
                alternate,
            } => self.evaluate_if(predicate, consequent, alternate, ctx),
            ExpressionKind::Else { consequent } => self.evaluate_expression(consequent, ctx),
            ExpressionKind::Ne { left, right } => self.evaluate_binary_ne(left, right, ctx),
            ExpressionKind::Eq { left, right } => self.evaluate_binary_eq(left, right, ctx),
            ExpressionKind::Sub { left, right } => self.evaluate_binary_subtract(left, right, ctx),
            ExpressionKind::Bool { value } => Ok(Value::Bool { value: *value }),
            ExpressionKind::String { value } => Ok(Value::String {
                value: value.clone(),
            }),
            ExpressionKind::Lt { left, right } => self.evaluate_binary_lt(left, right, ctx),
            ExpressionKind::Gt { left, right } => self.evaluate_binary_gt(left, right, ctx),
            ExpressionKind::Modulus { left, right } => self.evaluate_modulus(left, right, ctx),
            ExpressionKind::And { left, right } => self.evaluate_and(left, right, ctx),
            ExpressionKind::Or { left, right } => self.evaluate_or(left, right, ctx),
        }
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
