use std::collections::HashMap;

use crate::{
    parser::{Expression, ExpressionKind},
    symbol_table::SymbolTable,
};

#[derive(Debug, Clone)]
pub enum Value {
    Int { value: i64 },
    Bool { value: bool },
    Fn { function: ExpressionKind },
    Unit,
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError,
    UndefinedReference,
    UninitializedVariable,
    IllegalInvocation,
}

pub struct Interpreter {}

#[derive(Debug)]
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
            ExpressionKind::Identifier { name } => self.evaluate_identifier(name, ctx),
            ExpressionKind::Invocation { name, arguments } => {
                self.evaluate_invocation(name, arguments, ctx)
            }
            ExpressionKind::LetBinding { name, right } => self.evaluate_let(name, right, ctx),
            ExpressionKind::Int { value } => self.evaluate_int(value),
            ExpressionKind::BinaryAdd { left, right } => self.evaluate_binary_add(left, right, ctx),
            ExpressionKind::Block { body, return_val } => {
                self.evaluate_block(body, return_val, ctx)
            }
            ExpressionKind::If {
                predicate,
                if_block,
                else_block,
            } => self.evaluate_if(predicate, if_block, else_block, ctx),
            ExpressionKind::BinaryNe { left, right } => self.evaluate_binary_ne(left, right, ctx),
            ExpressionKind::BinaryEq { left, right } => self.evaluate_binary_eq(left, right, ctx),
            ExpressionKind::BinarySubtract { left, right } => {
                self.evaluate_binary_subtract(left, right, ctx)
            }
        }
    }

    fn evaluate_binary_subtract(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l - r,
                _ => return Err(RuntimeError::TypeError),
            },
            _ => return Err(RuntimeError::TypeError),
        };

        Ok(Value::Int { value: result })
    }

    fn evaluate_binary_ne(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l != r,
                _ => return Err(RuntimeError::TypeError),
            },
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l != r,
                _ => return Err(RuntimeError::TypeError),
            },
            _ => return Err(RuntimeError::TypeError),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_binary_eq(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let result = match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => l == r,
                _ => return Err(RuntimeError::TypeError),
            },
            Value::Bool { value: l } => match right {
                Value::Bool { value: r } => l == r,
                _ => return Err(RuntimeError::TypeError),
            },
            _ => return Err(RuntimeError::TypeError),
        };
        Ok(Value::Bool { value: result })
    }

    fn evaluate_if(
        &self,
        predicate: &Expression,
        if_block: &Expression,
        else_block: &Option<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = self.evaluate_expression(predicate, ctx)?;
        let Value::Bool { value } = value else {
            return Err(RuntimeError::TypeError);
        };

        if value {
            return self.evaluate_expression(if_block, ctx);
        };

        if let Some(else_block) = else_block {
            return self.evaluate_expression(else_block, ctx);
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
        callee: &str,
        arguments: &Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let value = ctx
            .bindings
            .get(callee)
            .ok_or(RuntimeError::UninitializedVariable)
            .map(|c| c.clone())?;

        let Value::Fn { function } = value else {
            return Err(RuntimeError::IllegalInvocation);
        };

        let ExpressionKind::Fn {
            parameters,
            body: block,
            ..
        } = function
        else {
            return Err(RuntimeError::TypeError);
        };

        let mut value_args = vec![];

        for arg in arguments {
            value_args.push(self.evaluate_expression(arg, ctx)?);
        }

        let mut bindings = ctx.bindings.clone();

        for (param, arg_value) in parameters.iter().zip(value_args.iter()) {
            bindings.insert(param.clone(), arg_value.clone());
        }

        let mut function_ctx = EvaluationContext {
            bindings,
            call_stack: ctx.call_stack.clone(),
            symbol_table: ctx.symbol_table.clone(),
        };

        self.evaluate_expression(&block, &mut function_ctx)
    }

    fn evaluate_identifier(
        &self,
        name: &str,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let declaration = ctx
            .symbol_table
            .get_declaration(name)
            .ok_or(RuntimeError::UndefinedReference)?;

        let value = ctx
            .bindings
            .get(declaration)
            .ok_or(RuntimeError::UninitializedVariable)?;

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
        Ok(Value::Int {
            value: value.clone(),
        })
    }

    fn evaluate_binary_add(
        &self,
        left: &Expression,
        right: &Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(left, ctx)?;
        let right = self.evaluate_expression(right, ctx)?;

        let Value::Int { value: l } = left else {
            return Err(RuntimeError::TypeError);
        };
        let Value::Int { value: r } = right else {
            return Err(RuntimeError::TypeError);
        };

        Ok(Value::Int { value: l + r })
    }
}
