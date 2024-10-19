use core::panic;
use std::collections::VecDeque;

use crate::{
    diagnostic::{Diagnostic, ErrorDiagnostic, InvalidSyntaxError, ToDiagnostic},
    lexer::{Token, TokenKind},
    location::{Location, Pos, WithLocation},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParserState {
    Default,
    Let,
}

pub struct Parser {
    tokens: VecDeque<Token>,
    last_location: Location,
    state: Vec<ParserState>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinExpressionKind {
    Print,
}

impl BuiltinExpressionKind {
    pub fn name(&self) -> String {
        match self {
            BuiltinExpressionKind::Print => "print",
        }
        .to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Mod,
    And,
    Or,
    Add,
    Sub,
    Lt,
    Gt,
    Ne,
    Eq,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub loc: Location,
    pub predicate: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Option<Expression>>,
}

#[derive(Debug, Clone)]
pub struct TypedIdentifierNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub type_expr: Option<TypeExpression>,
}

#[derive(Debug, Clone)]
pub struct ParameterNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub type_expr: TypeExpression,
}

#[derive(Debug, Clone)]
pub struct FnNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub parameters: Vec<ParameterNode>,
    pub body: Box<Expression>,
    pub return_type: Option<TypeExpression>,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub loc: Location,
    pub body: Vec<Expression>,
    pub return_val: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InvocationNode {
    pub loc: Location,
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct LetBindingNode {
    pub loc: Location,
    pub binding: TypedIdentifierNode,
    pub right: Box<Option<Expression>>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperationNode {
    pub loc: Location,
    pub operation: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode {
    pub loc: Location,
    pub indexee: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BuiltinNode {
    pub loc: Location,
    pub kind: BuiltinExpressionKind,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct IntTypeNode {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct StringTypeNode {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct BoolTypeNode {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode {
    pub loc: Location,
    pub elements: Box<TypeExpression>,
}

#[derive(Debug, Clone)]
pub struct ObjectPropertyTypeNode {
    pub loc: Location,
    pub name: PropertyNameNode,
    pub property_type: Box<TypeExpression>,
}

#[derive(Debug, Clone)]
pub struct IdentifierTypeNode {
    pub loc: Location,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct UnitTypeNode {
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct FnTypeNode {
    pub loc: Location,
    pub parameters: Vec<TypeExpression>,
    pub return_type: Box<TypeExpression>,
}

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Int(IntTypeNode),
    String(StringTypeNode),
    Bool(BoolTypeNode),
    Array(ArrayTypeNode),
    Unit(UnitTypeNode),
    Fn(FnTypeNode),
    Identifier(IdentifierTypeNode),
}

impl WithLocation for TypeExpression {
    fn loc(&self) -> Location {
        match self {
            TypeExpression::Int(t) => t.loc,
            TypeExpression::String(t) => t.loc,
            TypeExpression::Bool(t) => t.loc,
            TypeExpression::Array(t) => t.loc,
            TypeExpression::Identifier(t) => t.loc,
            TypeExpression::Unit(t) => t.loc,
            TypeExpression::Fn(t) => t.loc,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierNode {
    pub loc: Location,
    pub name: String,
}

impl IdentifierNode {
    pub fn to_string(&self) -> String {
        self.name.to_string()
    }
}

#[derive(Debug, Clone)]
pub enum ForLoopTarget {
    Single(IdentifierNode),
    Tuple(IdentifierNode, IdentifierNode),
}

#[derive(Debug, Clone)]
pub struct ForLoopNode {
    pub loc: Location,
    pub targets: ForLoopTarget,
    pub iterable: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PropertyNameNode {
    pub loc: Location,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct PropertyNode {
    pub loc: Location,
    pub name: PropertyNameNode,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ObjectInstantiationNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub properties: Vec<PropertyNode>,
}

#[derive(Debug, Clone)]
pub struct TypedPropertyNode {
    pub loc: Location,
    pub name: PropertyNameNode,
    pub type_expr: TypeExpression,
}

#[derive(Debug, Clone)]
pub struct StructDefinitionNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub properties: Vec<TypedPropertyNode>,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode {
    pub loc: Location,
    pub object: Box<Expression>,
    pub identifier: IdentifierNode,
}

#[derive(Debug, Clone)]
pub struct BoolNode {
    pub loc: Location,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct ArrayNode {
    pub loc: Location,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct ElseNode {
    pub loc: Location,
    pub consequent: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct StringNode {
    pub loc: Location,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct IntNode {
    pub loc: Location,
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct AssignmentNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UseNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Array(ArrayNode),
    Assignment(AssignmentNode),
    Binary(BinaryOperationNode),
    Block(BlockNode),
    Bool(BoolNode),
    Builtin(BuiltinNode),
    Else(ElseNode),
    Empty(Location),
    Fn(FnNode),
    ForLoop(ForLoopNode),
    Identifier(IdentifierNode),
    If(IfNode),
    IndexAccess(IndexAccessNode),
    Int(IntNode),
    Invocation(InvocationNode),
    LetBinding(LetBindingNode),
    ObjectInstantiation(ObjectInstantiationNode),
    PropertyAccess(PropertyAccessNode),
    String(StringNode),
    StructDefinitionNode(StructDefinitionNode),
    Use(UseNode),
}

impl Expression {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl WithLocation for Expression {
    fn loc(&self) -> Location {
        match self {
            Expression::Array(n) => n.loc,
            Expression::Binary(n) => n.loc,
            Expression::Block(n) => n.loc,
            Expression::Bool(n) => n.loc,
            Expression::Builtin(n) => n.loc,
            Expression::Else(n) => n.loc,
            Expression::Fn(n) => n.loc,
            Expression::ForLoop(n) => n.loc,
            Expression::Identifier(n) => n.loc,
            Expression::If(n) => n.loc,
            Expression::IndexAccess(n) => n.loc,
            Expression::Int(n) => n.loc,
            Expression::Invocation(n) => n.loc,
            Expression::LetBinding(n) => n.loc,
            Expression::ObjectInstantiation(n) => n.loc,
            Expression::PropertyAccess(n) => n.loc,
            Expression::String(n) => n.loc,
            Expression::Assignment(n) => n.loc,
            Expression::Use(n) => n.loc,
            Expression::StructDefinitionNode(n) => n.loc,

            Expression::Empty(loc) => *loc,
        }
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub loc: Location,
}

impl ToDiagnostic for SyntaxError {
    fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::Error(ErrorDiagnostic::InvalidSyntax(InvalidSyntaxError {
            loc: self.loc,
            message: self.kind.to_string(),
        }))
    }
}

impl SyntaxError {
    fn unexpected_token<R>(expected: Vec<TokenKind>, found: &Token) -> Result<R, SyntaxError> {
        Err(SyntaxError {
            loc: Location::new(found.start, found.end),
            kind: SyntaxErrorKind::UnexpectedToken {
                expected,
                found: found.kind.clone(),
            },
        })
    }
}

#[derive(Debug)]
pub enum SyntaxErrorKind {
    UnexpectedEndOfFile,
    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },
}

impl ToString for SyntaxErrorKind {
    fn to_string(&self) -> String {
        let str = match self {
            SyntaxErrorKind::UnexpectedEndOfFile => "Unexpected end of program",
            SyntaxErrorKind::UnexpectedToken { expected, found } => {
                let expected = expected
                    .into_iter()
                    .map(|t| format!("'{}'", t.to_string()))
                    .collect::<Vec<_>>()
                    .join(", ");
                &format!(
                    "Unexpected token '{}'  expected any of {expected}",
                    found.to_string(),
                )
            }
        };
        str.to_string()
    }
}

struct SignificantTokenClassifier {
    tokens: VecDeque<Token>,
}
impl SignificantTokenClassifier {
    pub fn new(tokens: Vec<Token>) -> SignificantTokenClassifier {
        SignificantTokenClassifier {
            tokens: VecDeque::from(tokens),
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn peek(&self) -> Option<&Token> {
        self.peek_at_offset(0)
    }

    fn peek_at_offset(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i)
    }

    fn peek_pair(&self) -> Option<(&Token, &Token)> {
        Some((self.peek()?, self.peek_at_offset(1)?))
    }

    fn next_pair_is(&self, tokens: (TokenKind, TokenKind)) -> bool {
        match self.peek_pair() {
            Some((a, b)) => a.kind == tokens.0 && b.kind == tokens.1,
            None => false,
        }
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    fn significants(&mut self) -> Vec<Token> {
        let mut tokens = vec![];

        loop {
            let next = self.next();
            match next {
                Some(first) => match first.kind {
                    TokenKind::Newline => continue,
                    TokenKind::WhiteSpace => continue,

                    TokenKind::ForwardSlash => match self.next() {
                        Some(second) => match second.kind {
                            TokenKind::ForwardSlash => {
                                while !self.next_is(TokenKind::Newline) {
                                    match self.next() {
                                        None => return tokens,
                                        Some(_) => (),
                                    }
                                }
                                continue;
                            }

                            TokenKind::Asterisk => {
                                self.next();
                                while !self
                                    .next_pair_is((TokenKind::Asterisk, TokenKind::ForwardSlash))
                                {
                                    match self.next() {
                                        Some(_) => (),
                                        None => return tokens,
                                    }
                                }
                                self.next();
                                self.next();
                                continue;
                            }
                            _ => tokens.push(second),
                        },
                        None => return tokens,
                    },
                    _ => tokens.push(first),
                },
                None => return tokens,
            }
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let tokens = SignificantTokenClassifier::new(tokens).significants();
        Parser {
            tokens: VecDeque::from(tokens),
            last_location: Location::new(Pos::start(), Pos::start()),
            state: vec![ParserState::Default],
        }
    }

    fn next(&mut self) -> Result<Token, SyntaxError> {
        let token = self.tokens.pop_front().ok_or(self.eof())?;
        self.last_location = (&token).into();
        Ok(token)
    }

    fn eof(&self) -> SyntaxError {
        SyntaxError {
            loc: self.last_location,
            kind: SyntaxErrorKind::UnexpectedEndOfFile,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn assert_next(&mut self, kind: TokenKind) -> Result<Token, SyntaxError> {
        let next = self.peek().ok_or(self.eof())?;
        if next.kind == kind {
            Ok(self.next()?)
        } else {
            SyntaxError::unexpected_token(vec![kind], &next)
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.peek_at_offset(0)
    }

    fn assert_peek(&self) -> Result<&Token, SyntaxError> {
        self.peek().ok_or(self.eof())
    }

    fn peek_at_offset(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i)
    }

    fn peek_pair(&self) -> Option<(&Token, &Token)> {
        Some((self.peek()?, self.peek_at_offset(1)?))
    }

    fn at_offset_is(&self, kind: TokenKind, offset: usize) -> bool {
        match self.peek_at_offset(offset) {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    fn next_is(&self, kind: TokenKind) -> bool {
        self.at_offset_is(kind, 0)
    }

    fn parse_expression(&mut self) -> Result<Expression, SyntaxError> {
        let expression = self.parse_accessor_expression()?;

        let Some(next) = self.peek() else {
            return Ok(expression);
        };

        let (kind, right) = match next.kind {
            TokenKind::Plus => {
                self.assert_next(TokenKind::Plus)?;
                (BinaryOp::Add, self.parse_expression()?)
            }
            TokenKind::Percentage => {
                self.assert_next(TokenKind::Percentage)?;
                (BinaryOp::Mod, self.parse_expression()?)
            }
            TokenKind::Ampersand => {
                self.assert_next(TokenKind::Ampersand)?;
                self.assert_next(TokenKind::Ampersand)?;
                (BinaryOp::And, self.parse_expression()?)
            }
            TokenKind::Pipe => {
                self.assert_next(TokenKind::Pipe)?;
                self.assert_next(TokenKind::Pipe)?;
                (BinaryOp::Or, self.parse_expression()?)
            }
            TokenKind::OpenAngleBracket => {
                self.assert_next(TokenKind::OpenAngleBracket)?;
                (BinaryOp::Lt, self.parse_expression()?)
            }
            TokenKind::ClosedAngleBracket => {
                self.assert_next(TokenKind::ClosedAngleBracket)?;
                (BinaryOp::Gt, self.parse_expression()?)
            }
            TokenKind::Minus => {
                self.assert_next(TokenKind::Minus)?;
                (BinaryOp::Sub, self.parse_expression()?)
            }
            TokenKind::Bang => {
                self.assert_next(TokenKind::Bang)?;
                self.assert_next(TokenKind::Equals)?;
                (BinaryOp::Ne, self.parse_expression()?)
            }
            TokenKind::Equals => match self.peek_pair() {
                Some((a, b)) => match (&a.kind, &b.kind) {
                    (TokenKind::Equals, TokenKind::Equals) => {
                        self.assert_next(TokenKind::Equals)?;
                        self.assert_next(TokenKind::Equals)?;
                        (BinaryOp::Eq, self.parse_expression()?)
                    }
                    _ => {
                        let Token { start, end, .. } = self.assert_next(TokenKind::Equals)?;
                        let Expression::Identifier(node) = expression else {
                            return Err(self.eof());
                        };

                        return Ok(Expression::Assignment(AssignmentNode {
                            loc: Location::new(start, end),
                            identifier: node,
                            right: self.parse_expression()?.boxed(),
                        }));
                    }
                },
                _ => return Ok(expression),
            },
            _ => return Ok(expression),
        };

        Ok(Expression::Binary(BinaryOperationNode {
            loc: Location::new(expression.loc().start, right.loc().end),
            operation: kind,
            left: expression.boxed(),
            right: right.boxed(),
        }))
    }

    fn parse_object_instantiation_node(&mut self) -> Result<ObjectInstantiationNode, SyntaxError> {
        let identifier = self.parse_identifier_node()?;
        let Token { start, .. } = self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut properties = vec![];

        loop {
            // If the first iteration returns an empty object
            if self.next_is(TokenKind::ClosedCurlyBrace) {
                break;
            }

            let name = self.parse_property_name_node()?;
            let value = if self.next_is(TokenKind::Colon) {
                self.assert_next(TokenKind::Colon)?;
                self.parse_expression()?
            } else {
                Expression::Identifier(IdentifierNode {
                    name: name.value.clone(),
                    loc: name.loc,
                })
            };

            properties.push(PropertyNode {
                loc: Location::new(name.loc.start, value.loc().end),
                name,
                value: value.boxed(),
            });

            if self.next_is(TokenKind::Comma) {
                self.next()?;
            } else {
                break;
            }
        }

        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(ObjectInstantiationNode {
            identifier,
            loc: Location::new(start, end),
            properties,
        })
    }

    fn parse_object_instantiation_or_identifier(&mut self) -> Result<Expression, SyntaxError> {
        // Look 1 step ahead since the identifier is parsed by the subsequent function calls
        // Eg, MyStruct {} should be parsed as an object instantiation
        // but MyStruct should be parsed as an identifier
        if self.at_offset_is(TokenKind::OpenCurlyBrace, 1) {
            let object_instantiation = self.parse_object_instantiation_node()?;
            Ok(Expression::ObjectInstantiation(object_instantiation))
        } else {
            self.parse_identifier()
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, SyntaxError> {
        Ok(Expression::Identifier(self.parse_identifier_node()?))
    }

    fn parse_type_expression(&mut self) -> Result<TypeExpression, SyntaxError> {
        let mut type_exp = self.parse_leaf_type_expression()?;

        loop {
            let Some(next) = self.peek() else {
                return Ok(type_exp);
            };

            type_exp = match next.kind {
                TokenKind::OpenSquareBracket => {
                    self.assert_next(TokenKind::OpenSquareBracket)?;
                    let Token { end, .. } = self.assert_next(TokenKind::ClosedSquareBracket)?;

                    TypeExpression::Array(ArrayTypeNode {
                        loc: Location::new(type_exp.loc().start, end),
                        elements: Box::new(type_exp),
                    })
                }
                _ => return Ok(type_exp),
            }
        }
    }

    fn parse_leaf_type_expression(&mut self) -> Result<TypeExpression, SyntaxError> {
        if self.next_is(TokenKind::Symbol) {
            let Token {
                lexeme, start, end, ..
            } = self.assert_next(TokenKind::Symbol)?;
            let loc = Location::new(start, end);
            let exp = match lexeme.as_str() {
                "int" => TypeExpression::Int(IntTypeNode { loc }),
                "string" => TypeExpression::String(StringTypeNode { loc }),
                "bool" => TypeExpression::Bool(BoolTypeNode { loc }),
                "unit" => TypeExpression::Unit(UnitTypeNode { loc }),
                _ => TypeExpression::Identifier(IdentifierTypeNode { loc, name: lexeme }),
            };
            return Ok(exp);
        }
        return self.parse_fn_type_expression();
    }

    fn parse_fn_type_expression(&mut self) -> Result<TypeExpression, SyntaxError> {
        let mut parameters = vec![];
        let Token { start, .. } = self.assert_next(TokenKind::OpenParen)?;

        loop {
            // If the first iteration returns an empty object
            if self.next_is(TokenKind::ClosedParen) {
                break;
            }

            parameters.push(self.parse_type_expression()?);

            if self.next_is(TokenKind::Comma) {
                self.next()?;
            } else {
                break;
            }
        }

        self.assert_next(TokenKind::ClosedParen)?;
        self.assert_next(TokenKind::Minus)?;
        self.assert_next(TokenKind::ClosedAngleBracket)?;
        let return_type = self.parse_type_expression()?;

        Ok(TypeExpression::Fn(FnTypeNode {
            loc: Location::new(start, return_type.loc().end),
            parameters,
            return_type: Box::new(return_type),
        }))
    }

    fn parse_accessor_expression(&mut self) -> Result<Expression, SyntaxError> {
        let mut expression = self.parse_leaf_expression()?;

        loop {
            let Some(next) = self.peek() else {
                return Ok(expression);
            };

            expression = match next.kind {
                TokenKind::OpenParen => Expression::Invocation(InvocationNode {
                    loc: expression.loc(),
                    callee: expression.boxed(),
                    arguments: self.parse_arguments()?,
                }),
                TokenKind::Dot => {
                    let Token { start, .. } = self.assert_next(TokenKind::Dot)?;
                    let property_name = self.parse_identifier_node()?;
                    Expression::PropertyAccess(PropertyAccessNode {
                        loc: Location::new(start, property_name.loc.end),
                        identifier: property_name,
                        object: expression.boxed(),
                    })
                }
                TokenKind::OpenSquareBracket => {
                    self.assert_next(TokenKind::OpenSquareBracket)?;
                    let exp = Expression::IndexAccess(IndexAccessNode {
                        loc: expression.loc(),
                        indexee: expression.boxed(),
                        index: self.parse_expression()?.boxed(),
                    });
                    self.assert_next(TokenKind::ClosedSquareBracket)?;
                    exp
                }
                _ => return Ok(expression),
            };
        }
    }

    fn parse_property_name_node(&mut self) -> Result<PropertyNameNode, SyntaxError> {
        let symbol = self.assert_next(TokenKind::Symbol)?;
        Ok(PropertyNameNode {
            loc: (&symbol).into(),
            value: symbol.lexeme,
        })
    }

    fn parse_leaf_expression(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.assert_peek()?;

        match self.state.last() {
            Some(state) => match state {
                ParserState::Default => match next.kind {
                    TokenKind::FnKeyword => self.parse_fn(),
                    TokenKind::StructKeyword => self.parse_struct_definition(),
                    TokenKind::LetKeyword => self.parse_let(),
                    TokenKind::UseKeyword => self.parse_use(),
                    TokenKind::ForKeyword => self.parse_for_loop(),

                    _ => self.parse_stateless_expression(),
                },
                ParserState::Let => self.parse_stateless_expression(),
            },
            None => panic!("internal neon error"),
        }
    }

    fn parse_stateless_expression(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.assert_peek()?;
        match next.kind {
            TokenKind::OpenSquareBracket => self.parse_array(),
            TokenKind::OpenCurlyBrace => self.parse_block(),
            TokenKind::FalseKeyword => self.parse_false_keyword(),
            TokenKind::TrueKeyword => self.parse_true_keyword(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::StringLiteral => self.parse_string(),
            TokenKind::IfKeyword => self.parse_if(),
            TokenKind::Symbol => self.parse_object_instantiation_or_identifier(),

            _ => SyntaxError::unexpected_token(
                vec![
                    TokenKind::OpenSquareBracket,
                    TokenKind::OpenCurlyBrace,
                    TokenKind::FalseKeyword,
                    TokenKind::TrueKeyword,
                    TokenKind::IntegerLiteral,
                    TokenKind::StringLiteral,
                    TokenKind::IfKeyword,
                    TokenKind::Symbol,
                ],
                next,
            ),
        }
    }

    fn parse_struct_definition(&mut self) -> Result<Expression, SyntaxError> {
        let structure = self.assert_next(TokenKind::StructKeyword)?;
        let identifier = self.parse_identifier_node()?;
        self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut properties = vec![];

        loop {
            // If the first iteration returns an empty object
            if self.next_is(TokenKind::ClosedCurlyBrace) {
                break;
            }

            let name = self.parse_property_name_node()?;
            let type_expr = self.parse_type_expression()?;
            properties.push(TypedPropertyNode {
                loc: Location::new(name.loc.start, type_expr.loc().end),
                type_expr,
                name,
            });

            if self.next_is(TokenKind::Comma) {
                self.next()?;
            } else {
                break;
            }
        }
        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression::StructDefinitionNode(StructDefinitionNode {
            loc: Location::new(structure.start, end),
            identifier,
            properties,
        }))
    }

    fn parse_use(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::UseKeyword)?;
        let identifier = self.parse_identifier_node()?;

        Ok(Expression::Use(UseNode {
            loc: Location::new(start, identifier.loc.end),
            identifier,
        }))
    }

    fn parse_for_loop(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::ForKeyword)?;

        let targets = self.parse_loop_targets()?;
        self.assert_next(TokenKind::InKeyword)?;
        let iterable = self.parse_expression()?.boxed();
        let body = self.parse_block()?.boxed();

        Ok(Expression::ForLoop(ForLoopNode {
            loc: Location::new(start, Pos::start()),
            targets,
            iterable,
            body,
        }))
    }

    fn parse_loop_targets(&mut self) -> Result<ForLoopTarget, SyntaxError> {
        if self.next_is(TokenKind::Symbol) {
            let node = self.parse_identifier_node()?;
            return Ok(ForLoopTarget::Single(node));
        }

        self.assert_next(TokenKind::OpenParen)?;
        let first = self.parse_identifier_node()?;
        self.assert_next(TokenKind::Comma)?;
        let second = self.parse_identifier_node()?;
        self.assert_next(TokenKind::ClosedParen)?;

        return Ok(ForLoopTarget::Tuple(first, second));
    }

    fn parse_optional_type(&mut self) -> Option<TypeExpression> {
        self.assert_next(TokenKind::Colon).ok()?;
        self.parse_type_expression().ok()
    }

    fn parse_mandatory_type(&mut self) -> Result<TypeExpression, SyntaxError> {
        self.assert_next(TokenKind::Colon)?;
        self.parse_type_expression()
    }

    fn parse_typed_identifier_node(&mut self) -> Result<TypedIdentifierNode, SyntaxError> {
        let identifier = self.parse_identifier_node()?;

        let typed = self.parse_optional_type();

        let end = typed
            .as_ref()
            .map(|t| t.loc().end)
            .unwrap_or(identifier.loc.end);

        Ok(TypedIdentifierNode {
            loc: Location::new((&identifier).loc.start, end),
            identifier,
            type_expr: typed,
        })
    }

    fn parse_identifier_node(&mut self) -> Result<IdentifierNode, SyntaxError> {
        let symbol = self.assert_next(TokenKind::Symbol)?;

        Ok(IdentifierNode {
            loc: (&symbol).into(),
            name: symbol.lexeme,
        })
    }

    fn parse_array(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::OpenSquareBracket)?;
        let mut elements = vec![];

        while !self.next_is(TokenKind::ClosedSquareBracket) {
            elements.push(self.parse_expression()?);

            if self.next_is(TokenKind::Comma) {
                self.next()?;
                continue;
            } else {
                break;
            }
        }

        let end = self.assert_next(TokenKind::ClosedSquareBracket)?.end;

        Ok(Expression::Array(ArrayNode {
            loc: Location::new(start, end),
            elements,
        }))
    }

    fn empty(&self) -> Expression {
        Expression::Empty(self.last_location)
    }

    fn parse_block_body(&mut self) -> Result<(Vec<Expression>, Expression), SyntaxError> {
        let mut body = vec![];
        let mut return_val = self.empty();
        loop {
            if self.next_is(TokenKind::ClosedCurlyBrace) || self.is_at_end() {
                break;
            };

            let expression = self.parse_expression()?;

            if self.next_is(TokenKind::ClosedCurlyBrace) || self.is_at_end() {
                return_val = expression;
                break;
            } else {
                body.push(expression);
            }
        }
        Ok((body, return_val))
    }

    fn parse_block(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::OpenCurlyBrace)?;
        let (body, return_val) = self.parse_block_body()?;
        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression::Block(BlockNode {
            loc: Location::new(start, end),
            return_val: return_val.boxed(),
            body,
        }))
    }

    pub fn parse_program(&mut self) -> Result<Expression, SyntaxError> {
        let (body, return_val) = self.parse_block_body()?;
        let start = body.first().unwrap_or(&return_val).loc().start;

        Ok(Expression::Block(BlockNode {
            loc: Location::new(start, return_val.loc().end),
            return_val: return_val.boxed(),
            body,
        }))
    }

    fn parse_string(&mut self) -> Result<Expression, SyntaxError> {
        let token = self.assert_next(TokenKind::StringLiteral)?;
        Ok(Expression::String(StringNode {
            loc: (&token).into(),
            value: token.lexeme,
        }))
    }

    fn parse_true_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let token = self.assert_next(TokenKind::TrueKeyword)?;
        Ok(Expression::Bool(BoolNode {
            value: true,
            loc: token.into(),
        }))
    }

    fn parse_false_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let token = self.assert_next(TokenKind::FalseKeyword)?;
        Ok(Expression::Bool(BoolNode {
            value: false,
            loc: token.into(),
        }))
    }

    fn parse_if(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::IfKeyword)?;
        let predicate = self.parse_expression()?;

        let consequent = self.parse_block()?;
        let alternate = self.parse_else().ok();

        let end = alternate
            .as_ref()
            .map(|a| a.loc().end)
            .unwrap_or(consequent.loc().end);

        Ok(Expression::If(IfNode {
            loc: Location::new(start, end),
            predicate: predicate.boxed(),
            consequent: consequent.boxed(),
            alternate: Box::new(alternate),
        }))
    }

    fn parse_else(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::ElseKeyword)?;

        let consequent = self.parse_if().ok().unwrap_or(self.parse_block()?);

        Ok(Expression::Else(ElseNode {
            loc: Location::new(start, consequent.loc().end),
            consequent: consequent.boxed(),
        }))
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>, SyntaxError> {
        let mut arguments = vec![];

        self.assert_next(TokenKind::OpenParen)?;

        loop {
            if self.next_is(TokenKind::ClosedParen) {
                break;
            }

            arguments.push(self.parse_expression()?);

            if self.next_is(TokenKind::ClosedParen) {
                break;
            } else {
                self.assert_next(TokenKind::Comma)?;
            }
        }
        self.assert_next(TokenKind::ClosedParen)?;
        return Ok(arguments);
    }

    fn parse_integer(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::IntegerLiteral)?;

        let value = lexeme.parse::<i64>().expect("Internal neon error");

        Ok(Expression::Int(IntNode {
            loc: Location::new(start, end),
            value,
        }))
    }

    fn parse_let(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::LetKeyword)?;
        let name = self.parse_typed_identifier_node()?;
        let mut right = None;
        if self.next_is(TokenKind::Equals) {
            self.assert_next(TokenKind::Equals)?;
            self.state.push(ParserState::Let);
            right = Some(self.parse_expression()?);
            self.state.pop();
        };

        let end = right.as_ref().map(|r| r.loc().end).unwrap_or(name.loc.end);

        Ok(Expression::LetBinding(LetBindingNode {
            binding: name,
            loc: Location::new(start, end),
            right: Box::new(right),
        }))
    }

    fn parse_parameters(&mut self) -> Result<Vec<ParameterNode>, SyntaxError> {
        self.assert_next(TokenKind::OpenParen)?;
        let mut params = vec![];

        loop {
            if self.next_is(TokenKind::ClosedParen) {
                break;
            }
            let identifier = self.parse_identifier_node()?;
            let typed = self.parse_mandatory_type()?;

            params.push(ParameterNode {
                loc: Location::new(identifier.loc.start, typed.loc().end),
                identifier,
                type_expr: typed,
            });

            if self.next_is(TokenKind::ClosedParen) {
                break;
            } else {
                self.assert_next(TokenKind::Comma)?;
            }
        }

        self.assert_next(TokenKind::ClosedParen)?;
        Ok(params)
    }

    fn parse_fn(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::FnKeyword)?;
        let name = self.parse_identifier_node()?;
        let parameters = self.parse_parameters()?;
        let return_type = self.parse_optional_type();

        let block = self.parse_block()?;

        Ok(Expression::Fn(FnNode {
            return_type,
            loc: Location::new(start, block.loc().end),
            identifier: name,
            parameters,
            body: block.boxed(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(str: &str) -> Expression {
        let tokens = Lexer::new(str).collect::<Vec<_>>();
        Parser::new(tokens)
            .parse_program()
            .expect(&format!("{str}"))
    }

    #[test]
    fn parsing() {
        let ast = parse("let a = 3 let b = 4 fn test() { a + b }");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("let a = 3 fn t() { 3 + 4 } t()");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("let a = 4 let b = a + a fn test() { b + b }");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("a");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("3 == 3");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("3 != 3");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("let a = d");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("a % d");
        assert!(matches!(ast, Expression::Block { .. }));
        let ast = parse("hello() + hello()");
        assert!(matches!(ast, Expression::Block { .. }));
    }

    #[test]
    fn single() {
        let ast = parse("let a = 3 + 3");
        assert!(matches!(ast, Expression::Block { .. }));
    }
}
