use std::collections::HashMap;

use crate::{
    diagnostic::{Diagnostic, DiagnosticKind, DiagnosticsList},
    location::Location,
    parser::{Expression, ExpressionKind},
    symbol_table::SymbolTable,
};

#[derive(PartialEq, Eq)]
pub enum TypeKind {
    Never,
    Int,
    Bool,
    String,
    Unit,
    TypeFn {
        inputs: Vec<TypeKind>,
        outputs: Box<TypeKind>,
    },
}

pub struct Type {
    kind: TypeKind,
}

impl Type {
    fn new(kind: TypeKind) -> Type {
        Type { kind }
    }
}

pub struct TypeEnvironment {
    pub symbol_table: SymbolTable,
    pub bindings: HashMap<String, Type>,
}

#[derive(Debug)]
pub struct TypeChecker<'a> {
    diagnostics_list: &'a mut DiagnosticsList,
    current_loc: Location,
}

impl TypeChecker<'_> {
    pub fn typeof_expression(
        &mut self,
        expression: &Expression,
        env: &mut TypeEnvironment,
    ) -> Type {
        self.current_loc = expression.loc;
        match &expression.kind {
            ExpressionKind::Fn(..) => self.typeof_fn(expression, env),
            ExpressionKind::Identifier(id) => self.typeof_identifier(id.name(), env),
            ExpressionKind::Invocation { .. } => self.typeof_invocation(expression, env),
            ExpressionKind::LetBinding { .. } => self.typeof_let(expression),
            ExpressionKind::Block { .. } => self.typeof_block(expression, env),
            ExpressionKind::If { .. } => self.typeof_if(expression),
            ExpressionKind::Else { .. } => self.typeof_expression(expression, env),

            ExpressionKind::Binary { .. } => self.typeof_binary(expression, env),
            ExpressionKind::Array { .. } => self.typeof_array(expression),
            ExpressionKind::IndexAccess { .. } => self.typeof_index_access(expression),
            ExpressionKind::Builtin { .. } => self.typeof_builtin(expression),
            ExpressionKind::Int { .. } => Type::new(TypeKind::Int),
            ExpressionKind::Bool { .. } => Type::new(TypeKind::Bool),
            ExpressionKind::String { .. } => Type::new(TypeKind::String),
            ExpressionKind::Empty => Type::new(TypeKind::Unit),
            ExpressionKind::ForLoop { .. } => todo!(),
            ExpressionKind::PropertyAccess { .. } => todo!(),
        }
    }

    fn typeof_builtin(&mut self, exp: &Expression) -> Type {
        todo!();
    }

    fn typeof_index_access(&mut self, exp: &Expression) -> Type {
        todo!();
    }

    fn typeof_array(&mut self, exp: &Expression) -> Type {
        todo!()
    }

    fn typeof_if(&mut self, exp: &Expression) -> Type {
        todo!()
    }

    fn typeof_fn(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!();
    }

    fn typeof_block(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_invocation(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_identifier(&mut self, identifier: &str, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn typeof_let(&mut self, exp: &Expression) -> Type {
        todo!()
    }

    fn typeof_binary(&mut self, exp: &Expression, env: &mut TypeEnvironment) -> Type {
        todo!()
    }

    fn unify(&mut self, lhs: Type, rhs: Type) -> Type {
        if lhs.kind == TypeKind::Never {
            self.diagnostics_list
                .add(Diagnostic::new(DiagnosticKind::Error, self.current_loc));
            return lhs;
        }
        return Type::new(TypeKind::Never);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn visit_ok() {
        let code = String::from("let a = 3; fn t(){ 3 + 4 }; t();");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_expression().expect("Should work");
        let mut st = SymbolTable::new();

        let res = st.visit_expression(&ast);
        assert!(res.is_ok());
    }

    #[test]
    fn visit_fail() {
        let code = String::from("fn t() {5}; f()");
        let tokens = Lexer::new(code).collect::<Vec<_>>();
        let ast = Parser::new(tokens).parse_program().expect("Should work");
        let mut st = SymbolTable::new();

        let res = st.visit_expression(&ast);

        assert!(res.is_err());
    }
}
