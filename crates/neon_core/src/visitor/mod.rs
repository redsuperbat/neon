use scanner::Scanner;

use crate::parser::{
    ArrayNode, ArrayTypeNode, AssignmentNode, BinaryOperationNode, BlockNode, BoolNode,
    BoolTypeNode, BuiltinNode, ElseNode, Expression, FnNode, FnTypeNode, ForLoopNode,
    IdentifierNode, IdentifierTypeNode, IfNode, IndexAccessNode, IntNode, IntTypeNode,
    InvocationNode, LetBindingNode, ObjectPropertyTypeNode, ParameterNode, PropertyAccessNode,
    PropertyNameNode, PropertyNode, StringNode, StringTypeNode, StructDefinitionNode,
    StructInstantiationNode, TypeExpression, TypedIdentifierNode, TypedPropertyNode, UnitTypeNode,
    UseNode,
};

pub trait Visitor {
    fn enter_expression(&mut self, _expression: &Expression) {}
    fn leave_expression(&mut self, _expression: &Expression) {}

    fn enter_block(&mut self, _block: &BlockNode) {}
    fn leave_block(&mut self, _block: &BlockNode) {}

    fn enter_fn(&mut self, _function: &FnNode) {}
    fn leave_fn(&mut self, _function: &FnNode) {}

    fn enter_if(&mut self, _if_node: &IfNode) {}
    fn leave_if(&mut self, _if_node: &IfNode) {}

    fn enter_invocation(&mut self, _invocation: &InvocationNode) {}
    fn leave_invocation(&mut self, _invocation: &InvocationNode) {}

    fn enter_let_binding(&mut self, _let_binding: &LetBindingNode) {}
    fn leave_let_binding(&mut self, _let_binding: &LetBindingNode) {}

    fn enter_binary_operation(&mut self, _binary_op: &BinaryOperationNode) {}
    fn leave_binary_operation(&mut self, _binary_op: &BinaryOperationNode) {}

    fn enter_index_access(&mut self, _index_access: &IndexAccessNode) {}
    fn leave_index_access(&mut self, _index_access: &IndexAccessNode) {}

    fn enter_builtin(&mut self, _builtin: &BuiltinNode) {}
    fn leave_builtin(&mut self, _builtin: &BuiltinNode) {}

    fn enter_for_loop(&mut self, _for_loop: &ForLoopNode) {}
    fn leave_for_loop(&mut self, _for_loop: &ForLoopNode) {}

    fn enter_struct_instantiation(&mut self, _struct_instantiation: &StructInstantiationNode) {}
    fn leave_struct_instantiation(&mut self, _struct_instantiation: &StructInstantiationNode) {}

    fn enter_struct_definition(&mut self, _struct_definition: &StructDefinitionNode) {}
    fn leave_struct_definition(&mut self, _struct_definition: &StructDefinitionNode) {}

    fn enter_property_access(&mut self, _property_access: &PropertyAccessNode) {}
    fn leave_property_access(&mut self, _property_access: &PropertyAccessNode) {}

    fn enter_array(&mut self, _array_node: &ArrayNode) {}
    fn leave_array(&mut self, _array_node: &ArrayNode) {}

    fn enter_else(&mut self, _else_node: &ElseNode) {}
    fn leave_else(&mut self, _else_node: &ElseNode) {}

    fn enter_assignment(&mut self, _assignment: &AssignmentNode) {}
    fn leave_assignment(&mut self, _assignment: &AssignmentNode) {}

    // Single methods for nodes without nested expressions
    fn visit_typed_identifier(&mut self, _typed_identifier: &TypedIdentifierNode) {}
    fn visit_parameter(&mut self, _parameter: &ParameterNode) {}
    fn visit_int_type(&mut self, _int_type: &IntTypeNode) {}
    fn visit_string_type(&mut self, _string_type: &StringTypeNode) {}
    fn visit_bool_type(&mut self, _bool_type: &BoolTypeNode) {}
    fn visit_array_type(&mut self, _array_type: &ArrayTypeNode) {}
    fn visit_object_property_type(&mut self, _object_property_type: &ObjectPropertyTypeNode) {}
    fn visit_identifier_type(&mut self, _identifier_type: &IdentifierTypeNode) {}
    fn visit_unit_type(&mut self, _unit_type: &UnitTypeNode) {}
    fn visit_fn_type(&mut self, _fn_type: &FnTypeNode) {}
    fn visit_type_expression(&mut self, _type_expr: &TypeExpression) {}
    fn visit_identifier(&mut self, _identifier: &IdentifierNode) {}
    fn visit_property_name(&mut self, _property_name: &PropertyNameNode) {}
    fn visit_property(&mut self, _property: &PropertyNode) {}
    fn visit_typed_property(&mut self, _typed_property: &TypedPropertyNode) {}
    fn visit_bool(&mut self, _bool_node: &BoolNode) {}
    fn visit_string(&mut self, _string_node: &StringNode) {}
    fn visit_int(&mut self, _int_node: &IntNode) {}
    fn visit_use(&mut self, _use_node: &UseNode) {}

    fn scanner(&mut self) -> Scanner<Self>
    where
        Self: Sized,
    {
        Scanner::new(self)
    }
}

pub mod paralell_visitor;
pub mod scanner;
