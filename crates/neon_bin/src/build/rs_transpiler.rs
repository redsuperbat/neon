use neon_core::parser::*;
use neon_core::types::type_checker::Type;
use neon_core::types::type_env::TypeEnvironment;

pub struct RustTranspiler<'a> {
    type_env: &'a mut TypeEnvironment,
}

impl RustTranspiler<'_> {
    pub fn new<'a>(type_env: &'a mut TypeEnvironment) -> RustTranspiler<'a> {
        RustTranspiler { type_env }
    }

    fn to_rust_type(&self, t: &Type) -> String {
        match t {
            Type::Int => "i64",
            Type::Bool => "bool",
            Type::String => "String",
            Type::Unit => "()",

            Type::Never => todo!(),
            Type::Struct(_) => todo!(),
            Type::Array(_) => todo!(),
            Type::Any => todo!(),
            Type::Fn(_) => "",
        }
        .to_string()
    }

    fn transpile_expression(&mut self, node: &Expression) -> String {
        match node {
            Expression::Fn(n) => self.transpile_fn(n),
            Expression::Block(n) => self.transpile_block(n),
            Expression::Int(n) => format!("{}", n.value),
            Expression::String(n) => format!("\"{}\".to_string()", n.value),
            Expression::Empty(..) => "".to_string(),
            Expression::If(n) => self.transpile_if(n),
            Expression::Binary(n) => self.transpile_binary(n),
            Expression::Identifier(n) => n.name.clone(),
            Expression::Invocation(n) => self.transpile_invocation(n),
            Expression::Else(n) => self.transpile_else(n),

            Expression::Array(_n) => todo!(),
            Expression::Assignment(_n) => todo!(),
            Expression::Bool(_n) => todo!(),
            Expression::Builtin(_n) => todo!(),
            Expression::ForLoop(_n) => todo!(),
            Expression::IndexAccess(_n) => todo!(),
            Expression::LetBinding(_n) => todo!(),
            Expression::StructInstantiation(_n) => todo!(),
            Expression::PropertyAccess(_n) => todo!(),
            Expression::StructDefinitionNode(_n) => todo!(),
            Expression::UnitBlock(_n) => todo!(),
        }
    }

    pub fn transpile_program(&mut self, node: &Expression) -> String {
        let result = self.transpile_expression(node);

        format!("fn main(){{\n{};\n}}", result)
    }

    fn transpile_block(&mut self, n: &BlockNode) -> String {
        self.type_env.enter_scope();
        let mut result = String::from("{");
        for exp in &n.body {
            result += &self.transpile_expression(exp);
        }
        result += &self.transpile_expression(&n.return_val);
        result += "}";
        self.type_env.exit_scope();
        return result;
    }

    fn transpile_fn(&mut self, n: &FnNode) -> String {
        let fn_type = self
            .type_env
            .get(&n.identifier.name)
            .expect("Internal neon error");
        let Type::Fn(fn_type) = fn_type else {
            panic!("Internal neon error")
        };
        let mut result = String::new();
        result += &format!("fn {}(", n.identifier.name);
        result += &n
            .parameters
            .iter()
            .zip(&fn_type.parameters)
            .map(|(p, t)| format!("{}:{}", p.identifier.name, self.to_rust_type(t)))
            .collect::<Vec<_>>()
            .join(",");
        result += ")";
        result += &format!(" -> {}", self.to_rust_type(&fn_type.return_type));
        result += &self.transpile_block(&n.body);
        result
    }

    fn transpile_if(&mut self, n: &IfNode) -> String {
        let mut result = String::from("if (");
        result += &self.transpile_expression(&n.predicate);
        result += "){";
        result += &self.transpile_expression(&n.consequent);
        result += "}";
        if let Some(alternate) = n.alternate.as_ref() {
            result += &self.transpile_expression(alternate);
        };
        result
    }

    fn transpile_binary(&mut self, n: &BinaryOperationNode) -> String {
        let middle = match n.operation {
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
        format!(
            "{} {} {}",
            self.transpile_expression(&n.left),
            middle,
            self.transpile_expression(&n.right)
        )
    }

    fn transpile_invocation(&mut self, n: &InvocationNode) -> String {
        let start = self.transpile_expression(&n.callee);
        let args = &n
            .arguments
            .iter()
            .map(|a| self.transpile_expression(a))
            .collect::<Vec<_>>()
            .join(",");
        format!("{}({})", start, args)
    }

    fn transpile_else(&mut self, n: &ElseNode) -> String {
        format!(" else {}", self.transpile_expression(&n.consequent))
    }
}
