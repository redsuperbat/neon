use crate::parser::{
    ArrayNode, AssignmentNode, BinaryOp, BinaryOperationNode, BlockNode, Expression, FnNode,
    IfNode, InvocationNode, LetBindingNode,
};

pub struct LuaCompiler {}

impl LuaCompiler {
    pub fn new() -> LuaCompiler {
        LuaCompiler {}
    }

    pub fn compile_expression(&self, expression: &Expression) -> String {
        match &expression {
            Expression::Array(node) => self.compile_array(node),
            Expression::Assignment(node) => self.compile_assignment(node),
            Expression::Binary(node) => self.compile_binary(node),
            Expression::Block(node) => self.compile_block(node),
            Expression::LetBinding(node) => self.compile_let_binding(node),
            Expression::Identifier(node) => node.name.clone(),
            Expression::Invocation(node) => self.compile_invocation(node),
            Expression::Fn(node) => self.compile_fn(node),
            Expression::If(node) => self.compile_if(node),

            Expression::ForLoop(node) => todo!("{:?}", node),

            Expression::Builtin(node) => todo!("{:?}", node),
            Expression::Else(node) => todo!("{:?}", node),
            Expression::Object(node) => todo!("{:?}", node),
            Expression::PropertyAccess(node) => todo!("{:?}", node),
            Expression::IndexAccess(node) => todo!("{:?}", node),

            Expression::Int(node) => format!("{}", node.value),
            Expression::Bool(node) => format!("{}", node.value),
            Expression::String(string_node) => format!("\"{}\"", string_node.value),
            Expression::Empty(..) => "".to_string(),
        }
    }

    fn compile_if(&self, node: &IfNode) -> String {
        let mut if_str = String::from("(function()if ");
        if_str += &self.compile_expression(&node.predicate);
        if_str += " then\n";
        if_str += &self.compile_expression(&node.consequent);
        if_str += "end\nend)()";
        if_str
    }

    fn compile_fn(&self, node: &FnNode) -> String {
        let mut fn_str = String::from("local function ");
        fn_str += &node.identifier.name;
        fn_str += "(";
        for (i, param) in node.parameters.iter().enumerate() {
            if i != 0 {
                fn_str += ","
            }
            fn_str += &param.identifier.name;
        }
        fn_str += ")\n";
        fn_str += &self.compile_expression(&node.body);
        fn_str += "\n";
        fn_str += "end";
        fn_str
    }

    fn compile_invocation(&self, node: &InvocationNode) -> String {
        let mut invo = self.compile_expression(&node.callee);
        invo += "(";
        for (i, arg) in node.arguments.iter().enumerate() {
            if i != 0 {
                invo += ","
            }
            invo += &self.compile_expression(arg);
        }
        invo += ")";
        invo
    }

    fn compile_let_binding(&self, node: &LetBindingNode) -> String {
        let mut str = String::from("local ");
        str += &node.binding.identifier.name;
        if let Some(right) = &node.right.as_ref() {
            str += "=";
            str += &self.compile_expression(right);
        };
        str
    }

    fn compile_block(&self, node: &BlockNode) -> String {
        let mut str = String::new();
        for statement in &node.body {
            str += &self.compile_expression(statement);
            str += "\n";
        }
        str += "return ";
        str += &self.compile_expression(&node.return_val);
        str
    }

    fn compile_assignment(&self, node: &AssignmentNode) -> String {
        let mut str = format!("{}=", node.identifier.name);
        str += &self.compile_expression(&node.right);
        str
    }

    fn compile_binary(&self, node: &BinaryOperationNode) -> String {
        let left = self.compile_expression(&node.left);
        let right = self.compile_expression(&node.right);
        let op = match node.operation {
            BinaryOp::Mod => "%",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::Ne => "!=",
            BinaryOp::Eq => "==",
        };
        format!("{}{}{}", left, op, right)
    }

    fn compile_array(&self, node: &ArrayNode) -> String {
        let mut arr = String::from("{");
        for (i, element) in node.elements.iter().enumerate() {
            if i != 0 {
                arr += ","
            }
            arr += &self.compile_expression(element);
        }
        arr += "}";
        arr
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn compile_lua() {
        let code = String::from(
            "
let a = 3 
a = true
a
",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let c = LuaCompiler::new();
        let lua = c.compile_expression(&ast);
        println!("{}", lua);
    }
}
