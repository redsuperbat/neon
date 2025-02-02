use std::collections::HashMap;

use super::*;

pub struct TestVisitor {
    expression_count: HashMap<ExpressionCheck, usize>,
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum ExpressionCheck {
    Else,
    Fn,
}

impl TestVisitor {
    pub fn new() -> TestVisitor {
        TestVisitor {
            expression_count: HashMap::new(),
        }
    }
    pub fn num_expressions(&self, exp: ExpressionCheck) -> usize {
        self.expression_count.get(&exp).unwrap_or(&0).clone()
    }
}

impl Visitor for TestVisitor {
    fn enter_else(&mut self, _n: &ElseNode) {
        *self
            .expression_count
            .entry(ExpressionCheck::Else)
            .or_insert(0) += 1;
    }

    fn enter_fn(&mut self, _n: &FnNode) {
        *self
            .expression_count
            .entry(ExpressionCheck::Fn)
            .or_insert(0) += 1;
    }
}
