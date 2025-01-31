use super::Visitor;
use crate::parser::*;

pub struct ParallelVisitor {
    visitors: Vec<Box<dyn Visitor>>,
}

impl Visitor for ParallelVisitor {
    fn enter_expression(&mut self, n: &Expression) {
        for visitor in &mut self.visitors {
            visitor.enter_expression(n);
        }
    }
    fn leave_expression(&mut self, n: &Expression) {
        for visitor in &mut self.visitors {
            visitor.leave_expression(n);
        }
    }
    fn enter_unit_block(&mut self, _expression: &UnitBlockNode) {
        for visitor in &mut self.visitors {
            visitor.enter_unit_block(_expression);
        }
    }
    fn leave_unit_block(&mut self, _expression: &UnitBlockNode) {
        for visitor in &mut self.visitors {
            visitor.leave_unit_block(_expression);
        }
    }

    fn enter_block(&mut self, n: &BlockNode) {
        for visitor in &mut self.visitors {
            visitor.enter_block(n);
        }
    }
    fn leave_block(&mut self, n: &BlockNode) {
        for visitor in &mut self.visitors {
            visitor.leave_block(n);
        }
    }
    fn enter_fn(&mut self, n: &FnNode) {
        for visitor in &mut self.visitors {
            visitor.enter_fn(n);
        }
    }
    fn leave_fn(&mut self, n: &FnNode) {
        for visitor in &mut self.visitors {
            visitor.leave_fn(n);
        }
    }
    fn enter_if(&mut self, n: &IfNode) {
        for visitor in &mut self.visitors {
            visitor.enter_if(n);
        }
    }
    fn leave_if(&mut self, n: &IfNode) {
        for visitor in &mut self.visitors {
            visitor.leave_if(n);
        }
    }
    fn enter_invocation(&mut self, n: &InvocationNode) {
        for visitor in &mut self.visitors {
            visitor.enter_invocation(n);
        }
    }
    fn leave_invocation(&mut self, n: &InvocationNode) {
        for visitor in &mut self.visitors {
            visitor.leave_invocation(n);
        }
    }
    fn enter_let_binding(&mut self, n: &LetBindingNode) {
        for visitor in &mut self.visitors {
            visitor.enter_let_binding(n)
        }
    }
    fn leave_let_binding(&mut self, n: &LetBindingNode) {
        for visitor in &mut self.visitors {
            visitor.leave_let_binding(n)
        }
    }
    fn enter_binary_operation(&mut self, n: &BinaryOperationNode) {
        for visitor in &mut self.visitors {
            visitor.enter_binary_operation(n)
        }
    }
    fn leave_binary_operation(&mut self, n: &BinaryOperationNode) {
        for visitor in &mut self.visitors {
            visitor.leave_binary_operation(n)
        }
    }
    fn enter_index_access(&mut self, n: &IndexAccessNode) {
        for visitor in &mut self.visitors {
            visitor.enter_index_access(n)
        }
    }
    fn leave_index_access(&mut self, n: &IndexAccessNode) {
        for visitor in &mut self.visitors {
            visitor.leave_index_access(n)
        }
    }
    fn enter_builtin(&mut self, n: &BuiltinNode) {
        for visitor in &mut self.visitors {
            visitor.enter_builtin(n)
        }
    }
    fn leave_builtin(&mut self, n: &BuiltinNode) {
        for visitor in &mut self.visitors {
            visitor.leave_builtin(n)
        }
    }
    fn enter_for_loop(&mut self, n: &ForLoopNode) {
        for visitor in &mut self.visitors {
            visitor.enter_for_loop(n)
        }
    }
    fn leave_for_loop(&mut self, n: &ForLoopNode) {
        for visitor in &mut self.visitors {
            visitor.leave_for_loop(n)
        }
    }
    fn enter_struct_instantiation(&mut self, n: &StructInstantiationNode) {
        for visitor in &mut self.visitors {
            visitor.enter_struct_instantiation(n)
        }
    }
    fn leave_struct_instantiation(&mut self, n: &StructInstantiationNode) {
        for visitor in &mut self.visitors {
            visitor.leave_struct_instantiation(n)
        }
    }
    fn enter_struct_definition(&mut self, n: &StructDefinitionNode) {
        for visitor in &mut self.visitors {
            visitor.enter_struct_definition(n)
        }
    }
    fn leave_struct_definition(&mut self, n: &StructDefinitionNode) {
        for visitor in &mut self.visitors {
            visitor.leave_struct_definition(n)
        }
    }
    fn enter_property_access(&mut self, n: &PropertyAccessNode) {
        for visitor in &mut self.visitors {
            visitor.enter_property_access(n)
        }
    }
    fn leave_property_access(&mut self, n: &PropertyAccessNode) {
        for visitor in &mut self.visitors {
            visitor.leave_property_access(n)
        }
    }
    fn enter_array(&mut self, n: &ArrayNode) {
        for visitor in &mut self.visitors {
            visitor.enter_array(n)
        }
    }
    fn leave_array(&mut self, n: &ArrayNode) {
        for visitor in &mut self.visitors {
            visitor.leave_array(n)
        }
    }
    fn enter_else(&mut self, n: &ElseNode) {
        for visitor in &mut self.visitors {
            visitor.enter_else(n)
        }
    }
    fn leave_else(&mut self, n: &ElseNode) {
        for visitor in &mut self.visitors {
            visitor.leave_else(n)
        }
    }
    fn enter_assignment(&mut self, n: &AssignmentNode) {
        for visitor in &mut self.visitors {
            visitor.enter_assignment(n)
        }
    }
    fn leave_assignment(&mut self, n: &AssignmentNode) {
        for visitor in &mut self.visitors {
            visitor.leave_assignment(n)
        }
    }
    fn visit_typed_identifier(&mut self, n: &TypedIdentifierNode) {
        for visitor in &mut self.visitors {
            visitor.visit_typed_identifier(n)
        }
    }
    fn visit_parameter(&mut self, n: &ParameterNode) {
        for visitor in &mut self.visitors {
            visitor.visit_parameter(n)
        }
    }
    fn visit_int_type(&mut self, n: &IntTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_int_type(n)
        }
    }
    fn visit_string_type(&mut self, n: &StringTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_string_type(n)
        }
    }
    fn visit_bool_type(&mut self, n: &BoolTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_bool_type(n)
        }
    }
    fn visit_array_type(&mut self, n: &ArrayTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_array_type(n)
        }
    }
    fn visit_object_property_type(&mut self, n: &ObjectPropertyTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_object_property_type(n)
        }
    }
    fn visit_identifier_type(&mut self, n: &IdentifierTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_identifier_type(n)
        }
    }
    fn visit_unit_type(&mut self, n: &UnitTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_unit_type(n)
        }
    }
    fn visit_fn_type(&mut self, n: &FnTypeNode) {
        for visitor in &mut self.visitors {
            visitor.visit_fn_type(n)
        }
    }
    fn visit_type_expression(&mut self, n: &TypeExpression) {
        for visitor in &mut self.visitors {
            visitor.visit_type_expression(n)
        }
    }
    fn visit_identifier(&mut self, n: &IdentifierNode) {
        for visitor in &mut self.visitors {
            visitor.visit_identifier(n)
        }
    }
    fn visit_property_name(&mut self, n: &PropertyNameNode) {
        for visitor in &mut self.visitors {
            visitor.visit_property_name(n)
        }
    }
    fn visit_property(&mut self, n: &PropertyNode) {
        for visitor in &mut self.visitors {
            visitor.visit_property(n)
        }
    }
    fn visit_typed_property(&mut self, n: &TypedPropertyNode) {
        for visitor in &mut self.visitors {
            visitor.visit_typed_property(n)
        }
    }
    fn visit_bool(&mut self, n: &BoolNode) {
        for visitor in &mut self.visitors {
            visitor.visit_bool(n)
        }
    }
    fn visit_string(&mut self, n: &StringNode) {
        for visitor in &mut self.visitors {
            visitor.visit_string(n)
        }
    }
    fn visit_int(&mut self, n: &IntNode) {
        for visitor in &mut self.visitors {
            visitor.visit_int(n)
        }
    }
    fn visit_use(&mut self, n: &UseNode) {
        for visitor in &mut self.visitors {
            visitor.visit_use(n)
        }
    }
    fn visit_struct_name(&mut self, n: &StructNameNode) {
        for visitor in &mut self.visitors {
            visitor.visit_struct_name(n);
        }
    }
}
