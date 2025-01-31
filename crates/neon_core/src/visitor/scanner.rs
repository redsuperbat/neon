use crate::parser::*;

use super::Visitor;

pub struct Scanner<'a, V: Visitor + ?Sized> {
    visitor: &'a mut V,
}

impl<'a, V: Visitor + ?Sized> Scanner<'a, V> {
    pub fn new(visitor: &'a mut V) -> Self {
        Self { visitor }
    }

    pub fn scan_expression(&mut self, expression: &Expression) {
        self.visitor.enter_expression(expression);
        match expression {
            Expression::Array(n) => self.scan_array(n),
            Expression::Assignment(n) => self.scan_assignment(n),
            Expression::Binary(n) => self.scan_binary(n),
            Expression::Block(n) => self.scan_block(n),
            Expression::Bool(n) => self.visitor.visit_bool(n),
            Expression::Else(n) => self.scan_else(n),
            Expression::Fn(n) => self.scan_fn(n),
            Expression::ForLoop(n) => self.scan_for_loop(n),
            Expression::Identifier(n) => self.visitor.visit_identifier(n),
            Expression::If(n) => self.scan_if(n),
            Expression::IndexAccess(n) => self.scan_index_access(n),
            Expression::Int(n) => self.visitor.visit_int(n),
            Expression::Invocation(n) => self.scan_invocation(n),
            Expression::LetBinding(n) => self.scan_let_binding(n),
            Expression::StructInstantiation(n) => self.scan_struct_instantiation(n),
            Expression::PropertyAccess(n) => self.scan_property_access(n),
            Expression::String(n) => self.visitor.visit_string(n),
            Expression::Use(n) => self.visitor.visit_use(n),
            Expression::StructDefinitionNode(n) => self.scan_struct_definition(n),
            Expression::UnitBlock(n) => self.scan_unit_block(n),

            Expression::Builtin(_) => todo!(),
            Expression::Empty(_) => {}
        }
        self.visitor.leave_expression(expression);
    }

    fn scan_array(&mut self, n: &ArrayNode) {
        self.visitor.enter_array(n);
        for element in &n.elements {
            self.scan_expression(element);
        }
        self.visitor.leave_array(n);
    }

    fn scan_assignment(&mut self, n: &AssignmentNode) {
        self.visitor.enter_assignment(n);
        self.scan_expression(&n.right);
        self.visitor.leave_assignment(n);
    }

    fn scan_binary(&mut self, n: &BinaryOperationNode) {
        self.visitor.enter_binary_operation(n);
        self.scan_expression(&n.left);
        self.scan_expression(&n.right);
        self.visitor.leave_binary_operation(n);
    }

    fn scan_block(&mut self, n: &BlockNode) {
        self.visitor.enter_block(n);
        for statement in &n.body {
            self.scan_expression(statement);
        }
        self.scan_expression(&n.return_val);
        self.visitor.leave_block(n);
    }

    fn scan_else(&mut self, n: &ElseNode) {
        self.visitor.enter_else(n);
        self.scan_expression(&n.consequent);
        self.visitor.leave_else(n);
    }

    fn scan_parameter(&mut self, n: &ParameterNode) {
        self.visitor.visit_parameter(n);
        self.visitor.visit_type_expression(&n.type_expr);
    }

    fn scan_fn(&mut self, n: &FnNode) {
        self.visitor.enter_fn(n);
        self.visitor.visit_identifier(&n.identifier);
        for param in &n.parameters {
            self.scan_parameter(param);
        }
        self.scan_block(&n.body);
        if let Some(return_type) = &n.return_type {
            self.visitor.visit_type_expression(return_type);
        }
        self.visitor.leave_fn(n);
    }

    fn scan_for_loop(&mut self, n: &ForLoopNode) {
        self.visitor.enter_for_loop(n);
        match &n.targets {
            ForLoopTarget::Single(n) => self.visitor.visit_identifier(&n),
            ForLoopTarget::Tuple(a, b) => {
                self.visitor.visit_identifier(&a);
                self.visitor.visit_identifier(&b);
            }
        }
        self.scan_expression(&n.iterable);
        self.scan_unit_block(&n.unit_block);
        self.visitor.leave_for_loop(n);
    }

    fn scan_if(&mut self, n: &IfNode) {
        self.visitor.enter_if(n);
        self.scan_expression(&n.predicate);
        self.scan_expression(&n.consequent);
        if let Some(alternate) = n.alternate.as_ref() {
            self.scan_expression(alternate);
        }
        self.visitor.leave_if(n);
    }

    fn scan_index_access(&mut self, n: &IndexAccessNode) {
        self.visitor.enter_index_access(n);
        self.scan_expression(&n.indexee);
        self.scan_expression(&n.index);
        self.visitor.leave_index_access(n);
    }

    fn scan_invocation(&mut self, n: &InvocationNode) {
        self.visitor.enter_invocation(n);
        self.scan_expression(&n.callee);
        for argument in &n.arguments {
            self.scan_expression(argument);
        }
        self.visitor.leave_invocation(n);
    }

    fn scan_let_binding(&mut self, n: &LetBindingNode) {
        self.visitor.enter_let_binding(n);
        self.visitor.visit_identifier(&n.binding.identifier);
        if let Some(type_expr) = &n.binding.type_expr {
            self.visitor.visit_type_expression(type_expr);
        }
        if let Some(value) = n.right.as_ref() {
            self.scan_expression(value);
        }
        self.visitor.leave_let_binding(n);
    }

    fn scan_struct_instantiation(&mut self, n: &StructInstantiationNode) {
        self.visitor.enter_struct_instantiation(n);
        self.visitor.visit_identifier(&n.identifier);
        for argument in &n.arguments {
            self.scan_expression(&argument.value);
        }
        self.visitor.leave_struct_instantiation(n);
    }

    fn scan_property_access(&mut self, n: &PropertyAccessNode) {
        self.visitor.enter_property_access(n);
        self.scan_expression(&n.object);
        self.visitor.visit_property_name(&n.property);
        self.visitor.leave_property_access(n);
    }

    fn scan_struct_definition(&mut self, n: &StructDefinitionNode) {
        self.visitor.enter_struct_definition(n);
        self.visitor.visit_struct_name(&n.name);
        for property in &n.properties {
            self.visitor.visit_type_expression(&property.type_expr);
        }
        self.visitor.leave_struct_definition(n);
    }

    fn scan_unit_block(&mut self, n: &UnitBlockNode) {
        self.visitor.enter_unit_block(n);
        n.statements
            .iter()
            .for_each(|statement| self.scan_expression(statement));
        self.visitor.leave_unit_block(n);
    }
}
