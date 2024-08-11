use crate::parser::{Expression, ExpressionKind};

#[derive(Debug)]
pub enum Value {
    Int { value: i64 },
    Fn { function: ExpressionKind },
    Unit,
}

#[derive(Debug)]
pub enum RuntimeError {
    TypeError { expected: Value, received: Value },
}

pub struct Interpreter {}

pub struct EvaluationContext {}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate_expression(
        &self,
        expression: Expression,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        match expression.kind {
            ExpressionKind::Fn {
                name,
                parameters,
                body,
            } => self.evaluate_fn(name, parameters, body, ctx),
            ExpressionKind::Identifier { name } => self.evaluate_identifier(name, ctx),
            ExpressionKind::Invocation { name, arguments } => {
                self.evaluate_invocation(name, arguments, ctx)
            }
            ExpressionKind::LetBinding { name, right } => self.evaluate_let(name, right),
            ExpressionKind::Int { value } => self.evaluate_int(value),
            ExpressionKind::BinaryAdd { left, right } => self.evaluate_binary_add(left, right, ctx),
            ExpressionKind::Program { body } => self.evaluate_program(body, ctx),
        }
    }

    fn evaluate_program(
        &self,
        body: Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        for exp in body {
            self.evaluate_expression(exp, ctx)?;
        }
        Ok(Value::Unit)
    }

    fn evaluate_fn(
        &self,
        name: String,
        parameters: Vec<String>,
        body: Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Fn {
            function: ExpressionKind::Fn {
                name,
                parameters,
                body,
            },
        })
    }

    fn evaluate_invocation(
        &self,
        name: String,
        arguments: Vec<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        todo!()
    }

    fn evaluate_identifier(
        &self,
        name: String,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        todo!()
    }

    fn evaluate_let(&self, _name: String, right: Box<Expression>) -> Result<Value, RuntimeError> {
        Ok(Value::Unit)
    }

    fn evaluate_int(&self, value: i64) -> Result<Value, RuntimeError> {
        Ok(Value::Int { value })
    }

    fn evaluate_binary_add(
        &self,
        left: Box<Expression>,
        right: Box<Expression>,
        ctx: &mut EvaluationContext,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate_expression(*left, ctx)?;
        let right = self.evaluate_expression(*right, ctx)?;
        match left {
            Value::Int { value: l } => match right {
                Value::Int { value: r } => Ok(Value::Int { value: l + r }),
                _ => Err(RuntimeError::TypeError {
                    expected: Value::Int { value: 0 },
                    received: right,
                }),
            },
            _ => Err(RuntimeError::TypeError {
                expected: Value::Int { value: 0 },
                received: left,
            }),
        }
    }
}
