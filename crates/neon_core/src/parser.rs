use std::collections::VecDeque;

use crate::{
    lexer::{Token, TokenKind},
    location::{Location, Pos},
};

pub struct Parser {
    tokens: VecDeque<Token>,
    last_location: Location,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub loc: Location,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn kind(kind: ExpressionKind) -> Expression {
        Expression {
            kind,
            loc: Location::beginning(),
        }
    }

    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
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
    pub predicate: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Option<Expression>>,
}

#[derive(Debug, Clone)]
pub struct FnNode {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BlockNode {
    pub body: Vec<Expression>,
    pub return_val: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct InvocationNode {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct LetBinding {
    pub name: String,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BinaryOperationNode {
    pub operation: BinaryOp,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode {
    pub indexee: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct BuiltinNode {
    pub kind: BuiltinExpressionKind,
    pub arguments: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct IdentifierNode(pub String);

impl IdentifierNode {
    pub fn name(&self) -> &str {
        &self.0
    }
    pub fn to_string(&self) -> String {
        self.name().to_string()
    }
}

#[derive(Debug, Clone)]
pub enum ForLoopTarget {
    Single(IdentifierNode),
    Tuple(IdentifierNode, IdentifierNode),
}

#[derive(Debug, Clone)]
pub struct ForLoopNode {
    pub targets: ForLoopTarget,
    pub iterable: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PropertyNode {
    pub identifier: IdentifierNode,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ObjectNode(pub Vec<PropertyNode>);

#[derive(Debug, Clone)]
pub struct PropertyAccessNode {
    pub object: Box<Expression>,
    pub property_name: IdentifierNode,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Array(Vec<Expression>),
    Binary(BinaryOperationNode),
    Block(BlockNode),
    Bool(bool),
    Builtin(BuiltinNode),
    Else(Box<Expression>),
    Empty,
    Fn(FnNode),
    ForLoop(ForLoopNode),
    Identifier(IdentifierNode),
    If(IfNode),
    IndexAccess(IndexAccessNode),
    Int(i64),
    Invocation(InvocationNode),
    LetBinding(LetBinding),
    Object(ObjectNode),
    PropertyAccess(PropertyAccessNode),
    String(String),
}

impl ExpressionKind {
    pub fn into_exp(self, loc: Location) -> Expression {
        Expression { kind: self, loc }
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub start: Pos,
    pub end: Pos,
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

impl SyntaxError {
    fn new<R>(expected: Vec<TokenKind>, found: &Token) -> Result<R, SyntaxError> {
        Err(SyntaxError {
            start: found.start,
            end: found.end,
            kind: SyntaxErrorKind::UnexpectedToken {
                expected,
                found: found.kind.clone(),
            },
        })
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: VecDeque::from(tokens),
            last_location: Location::new(Pos::start(), Pos::start()),
        }
    }

    fn next(&mut self) -> Result<Token, SyntaxError> {
        let token = self.tokens.pop_front().ok_or(SyntaxError {
            start: self.last_location.start,
            end: self.last_location.end,
            kind: SyntaxErrorKind::UnexpectedEndOfFile,
        })?;
        self.last_location = Location::new(token.start, token.end);
        Ok(token)
    }

    fn next_significant(&mut self) -> Result<Token, SyntaxError> {
        if self.next_sequence_is((TokenKind::ForwardSlash, TokenKind::Asterix)) {
            self.parse_comment()?;
        }

        let mut token = self.next()?;

        while token.kind == TokenKind::WhiteSpace {
            token = self.next()?;
        }

        while token.kind == TokenKind::Newline {
            token = self.next()?;
        }

        Ok(token)
    }

    fn parse_comment(&mut self) -> Result<(), SyntaxError> {
        // remove /*
        self.next()?;
        self.next()?;

        while !self.next_sequence_is((TokenKind::Asterix, TokenKind::ForwardSlash)) {
            self.next()?;
        }

        // remove */
        self.next()?;
        self.next()?;

        Ok(())
    }

    fn is_at_end(&mut self) -> bool {
        self.peek_significant().is_none()
    }

    fn assert_next(&mut self, kind: TokenKind) -> Result<Token, SyntaxError> {
        let next = self.next_significant()?;
        if next.kind == kind {
            Ok(next)
        } else {
            SyntaxError::new(vec![kind], &next)
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.peek_at_offset(0)
    }

    fn peek_significant(&mut self) -> Option<&Token> {
        if self.next_sequence_is((TokenKind::ForwardSlash, TokenKind::Asterix)) {
            self.parse_comment().ok()?;
        }

        let mut token = self.peek()?;

        while token.kind == TokenKind::WhiteSpace {
            self.next().ok()?;
            token = self.peek()?;
        }

        while token.kind == TokenKind::Newline {
            self.next().ok()?;
            token = self.peek()?;
        }

        self.peek()
    }

    fn peek_at_offset(&self, i: usize) -> Option<&Token> {
        self.tokens.get(i)
    }

    fn next_sequence_is(&self, tokens: (TokenKind, TokenKind)) -> bool {
        match (self.peek(), self.peek_at_offset(1)) {
            (Some(a), Some(b)) => a.kind == tokens.0 && b.kind == tokens.1,
            _ => false,
        }
    }

    fn next_is(&mut self, kind: TokenKind) -> bool {
        match self.peek_significant() {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    fn at_offet_is(&self, offset: usize, kind: TokenKind) -> bool {
        match self.peek_at_offset(offset) {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, SyntaxError> {
        let expression = self.parse_accessor_expression()?;

        let Some(next) = self.peek_significant() else {
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
            TokenKind::Equals => {
                if self.at_offet_is(1, TokenKind::Equals) {
                    self.assert_next(TokenKind::Equals)?;
                    self.assert_next(TokenKind::Equals)?;
                    (BinaryOp::Eq, self.parse_expression()?)
                } else {
                    return Ok(expression);
                }
            }
            _ => return Ok(expression),
        };

        Ok(Expression {
            loc: Location::new(expression.loc.start, right.loc.end),
            kind: ExpressionKind::Binary(BinaryOperationNode {
                operation: kind,
                left: expression.boxed(),
                right: right.boxed(),
            }),
        })
    }

    fn parse_identifier(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::Symbol)?;
        Ok(Expression {
            kind: ExpressionKind::Identifier(IdentifierNode(lexeme)),
            loc: Location::new(start, end),
        })
    }

    fn parse_accessor_expression(&mut self) -> Result<Expression, SyntaxError> {
        let mut expression = self.parse_leaf_expression()?;

        loop {
            let Some(next) = self.peek_significant() else {
                return Ok(expression);
            };

            expression = match next.kind {
                TokenKind::OpenParen => Expression {
                    loc: expression.loc,
                    kind: ExpressionKind::Invocation(InvocationNode {
                        callee: expression.boxed(),
                        arguments: self.parse_arguments()?,
                    }),
                },
                TokenKind::Dot => {
                    let Token { start, .. } = self.assert_next(TokenKind::Dot)?;
                    let Token { lexeme, .. } = self.assert_next(TokenKind::Symbol)?;
                    Expression {
                        loc: Location::new(start, expression.loc.end),
                        kind: ExpressionKind::PropertyAccess(PropertyAccessNode {
                            property_name: IdentifierNode(lexeme),
                            object: expression.boxed(),
                        }),
                    }
                }
                TokenKind::OpenSquareBracket => {
                    self.assert_next(TokenKind::OpenSquareBracket)?;
                    let exp = Expression {
                        loc: expression.loc,
                        kind: ExpressionKind::IndexAccess(IndexAccessNode {
                            indexee: expression.boxed(),
                            index: self.parse_expression()?.boxed(),
                        }),
                    };
                    self.assert_next(TokenKind::ClosedSquareBracket)?;
                    exp
                }
                _ => return Ok(expression),
            };
        }
    }

    fn parse_object(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut properties = vec![];

        loop {
            // If the first iteration returns an empty object
            if self.next_is(TokenKind::ClosedCurlyBrace) {
                break;
            }

            let Token { lexeme, .. } = self.assert_next(TokenKind::Symbol)?;
            self.assert_next(TokenKind::Colon)?;
            properties.push(PropertyNode {
                identifier: IdentifierNode(lexeme),
                value: self.parse_expression()?.boxed(),
            });

            if self.next_is(TokenKind::Comma) {
                self.next()?;
            } else {
                break;
            }
        }

        self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression {
            loc: Location::new(start, Pos::start()),
            kind: ExpressionKind::Object(ObjectNode(properties)),
        })
    }

    fn parse_leaf_expression(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.peek_significant();

        let Some(next) = next else {
            return Ok(Expression {
                kind: ExpressionKind::Empty,
                loc: self.last_location,
            });
        };

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::OpenSquareBracket => self.parse_array(),
            TokenKind::OpenCurlyBrace => self.parse_object(),
            TokenKind::ClosedCurlyBrace => self.parse_empty(),
            TokenKind::FalseKeyword => self.parse_false_keyword(),
            TokenKind::TrueKeyword => self.parse_true_keyword(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::StringLiteral => self.parse_string(),
            TokenKind::IfKeyword => self.parse_if(),
            TokenKind::LetKeyword => self.parse_let(),
            TokenKind::Symbol => self.parse_identifier(),
            TokenKind::ForKeyword => self.parse_for_loop(),
            _ => SyntaxError::new(
                vec![
                    TokenKind::FnKeyword,
                    TokenKind::OpenSquareBracket,
                    TokenKind::ClosedCurlyBrace,
                    TokenKind::FalseKeyword,
                    TokenKind::TrueKeyword,
                    TokenKind::IntegerLiteral,
                    TokenKind::StringLiteral,
                    TokenKind::IfKeyword,
                    TokenKind::LetKeyword,
                    TokenKind::Symbol,
                ],
                next,
            ),
        }
    }

    fn parse_for_loop(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::ForKeyword)?;

        let targets = self.parse_loop_targets()?;
        self.assert_next(TokenKind::InKeyword)?;
        let iterable = self.parse_expression()?.boxed();
        let body = self.parse_block()?.boxed();

        Ok(ExpressionKind::ForLoop(ForLoopNode {
            targets,
            iterable,
            body,
        })
        .into_exp(Location::new(start, Pos::start())))
    }

    fn parse_loop_targets(&mut self) -> Result<ForLoopTarget, SyntaxError> {
        if self.next_is(TokenKind::Symbol) {
            let node = self.parse_identifer_node()?;
            return Ok(ForLoopTarget::Single(node));
        }

        self.assert_next(TokenKind::OpenParen)?;
        let first = self.parse_identifer_node()?;
        self.assert_next(TokenKind::Comma)?;
        let second = self.parse_identifer_node()?;
        self.assert_next(TokenKind::ClosedParen)?;

        return Ok(ForLoopTarget::Tuple(first, second));
    }

    fn parse_identifer_node(&mut self) -> Result<IdentifierNode, SyntaxError> {
        let Token { lexeme, .. } = self.assert_next(TokenKind::Symbol)?;
        Ok(IdentifierNode(lexeme))
    }

    fn parse_array(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::OpenSquareBracket)?;
        let mut elements = vec![];

        while !self.next_is(TokenKind::ClosedSquareBracket) {
            elements.push(self.parse_expression()?);

            if self.next_is(TokenKind::Comma) {
                self.next_significant()?;
                continue;
            } else {
                break;
            }
        }

        let end = self.assert_next(TokenKind::ClosedSquareBracket)?.end;

        Ok(Expression {
            loc: Location::new(start, end),
            kind: ExpressionKind::Array(elements),
        })
    }

    fn parse_block_body(&mut self) -> Result<(Vec<Expression>, Expression), SyntaxError> {
        let mut body = vec![];
        let mut return_val = Expression {
            loc: self.last_location,
            kind: ExpressionKind::Empty,
        };
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

        Ok(Expression {
            loc: Location::new(start, end),
            kind: ExpressionKind::Block(BlockNode {
                return_val: return_val.boxed(),
                body,
            }),
        })
    }

    pub fn parse_program(&mut self) -> Result<Expression, SyntaxError> {
        let (body, return_val) = self.parse_block_body()?;
        let start = body.first().unwrap_or(&return_val).loc.start;

        Ok(Expression {
            loc: Location::new(start, return_val.loc.end),
            kind: ExpressionKind::Block(BlockNode {
                body,
                return_val: return_val.boxed(),
            }),
        })
    }

    fn parse_empty(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;
        Ok(Expression {
            kind: ExpressionKind::Empty,
            loc: Location::new(start, end),
        })
    }

    fn parse_string(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            start, end, lexeme, ..
        } = self.assert_next(TokenKind::StringLiteral)?;
        Ok(Expression {
            kind: ExpressionKind::String(lexeme),
            loc: Location::new(start, end),
        })
    }

    fn parse_true_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::TrueKeyword)?;
        Ok(Expression {
            kind: ExpressionKind::Bool(true),
            loc: Location::new(start, end),
        })
    }

    fn parse_false_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::FalseKeyword)?;
        Ok(Expression {
            kind: ExpressionKind::Bool(false),
            loc: Location::new(start, end),
        })
    }

    fn parse_if(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::IfKeyword)?;
        let predicate = self.parse_expression()?;

        let consequent = self.parse_block()?;
        let mut alternate = None;

        if self.next_is(TokenKind::ElseKeyword) {
            alternate = Some(self.parse_else()?);
        }

        Ok(Expression {
            loc: Location::new(start, consequent.loc.end),
            kind: ExpressionKind::If(IfNode {
                predicate: predicate.boxed(),
                consequent: consequent.boxed(),
                alternate: Box::new(alternate),
            }),
        })
    }

    fn parse_else(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::ElseKeyword)?;

        let consequent;

        if self.next_is(TokenKind::IfKeyword) {
            consequent = self.parse_if()?;
        } else {
            consequent = self.parse_block()?;
        }

        Ok(Expression {
            loc: Location::new(start, consequent.loc.end),
            kind: ExpressionKind::Else(consequent.boxed()),
        })
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

        Ok(Expression {
            loc: Location::new(start, end),
            kind: ExpressionKind::Int(value),
        })
    }

    fn parse_let(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::LetKeyword)?;
        let Token { lexeme, end, .. } = self.assert_next(TokenKind::Symbol)?;
        self.assert_next(TokenKind::Equals)?;
        let right = self.parse_expression().map(Box::new)?;

        Ok(Expression {
            kind: ExpressionKind::LetBinding(LetBinding {
                name: lexeme,
                right,
            }),
            loc: Location::new(start, end),
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<String>, SyntaxError> {
        self.assert_next(TokenKind::OpenParen)?;
        let mut params = vec![];

        loop {
            if self.next_is(TokenKind::ClosedParen) {
                break;
            }

            let name = self.assert_next(TokenKind::Symbol)?;
            params.push(name.lexeme);

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
        let name = self.assert_next(TokenKind::Symbol)?;
        let parameters = self.parse_parameters()?;

        let block = self.parse_block()?;

        Ok(Expression {
            loc: Location::new(start, block.loc.end),
            kind: ExpressionKind::Fn(FnNode {
                name: name.lexeme,
                parameters,
                body: block.boxed(),
            }),
        })
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
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("let a = 3 fn t() { 3 + 4 } t()");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("let a = 4 let b = a + a fn test() { b + b }");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("a");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("3 == 3");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("3 != 3");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("let a = d");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("a % d");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("hello() + hello()");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
    }

    #[test]
    fn single() {
        let ast = parse("let a = 3 + 3");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
    }
}
