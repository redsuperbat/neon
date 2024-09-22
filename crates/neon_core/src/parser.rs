use std::collections::VecDeque;

use crate::{
    lexer::{Token, TokenKind},
    location::{Location, Pos},
};

pub struct Parser {
    tokens: VecDeque<Token>,
    last_location: Location,
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
pub struct FnNode {
    pub loc: Location,
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Box<Expression>,
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
    pub name: String,
    pub right: Box<Expression>,
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
    pub arguments: Vec<String>,
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
pub struct PropertyNode {
    pub loc: Location,
    pub identifier: IdentifierNode,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ObjectNode {
    pub properties: Vec<PropertyNode>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode {
    pub loc: Location,
    pub object: Box<Expression>,
    pub property_name: IdentifierNode,
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
    Object(ObjectNode),
    PropertyAccess(PropertyAccessNode),
    String(StringNode),
}

impl Expression {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn loc(&self) -> Location {
        match self {
            Expression::Array(a) => a.loc,
            Expression::Binary(b) => b.loc,
            Expression::Block(e) => e.loc,
            Expression::Bool(e) => e.loc,
            Expression::Builtin(e) => e.loc,
            Expression::Else(e) => e.loc,
            Expression::Fn(e) => e.loc,
            Expression::ForLoop(e) => e.loc,
            Expression::Identifier(e) => e.loc,
            Expression::If(e) => e.loc,
            Expression::IndexAccess(e) => e.loc,
            Expression::Int(e) => e.loc,
            Expression::Invocation(e) => e.loc,
            Expression::LetBinding(e) => e.loc,
            Expression::Object(e) => e.loc,
            Expression::PropertyAccess(e) => e.loc,
            Expression::String(e) => e.loc,
            Expression::Empty(loc) => *loc,
            Expression::Assignment(e) => e.loc,
        }
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

                            TokenKind::Asterix => {
                                self.next();
                                while !self
                                    .next_pair_is((TokenKind::Asterix, TokenKind::ForwardSlash))
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
        }
    }

    fn next(&mut self) -> Result<Token, SyntaxError> {
        let token = self.tokens.pop_front().ok_or(self.eof())?;
        self.last_location = (&token).into();
        Ok(token)
    }

    fn eof(&self) -> SyntaxError {
        SyntaxError {
            start: self.last_location.start,
            end: self.last_location.end,
            kind: SyntaxErrorKind::UnexpectedEndOfFile,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn assert_next(&mut self, kind: TokenKind) -> Result<Token, SyntaxError> {
        let next = self.next()?;
        if next.kind == kind {
            Ok(next)
        } else {
            SyntaxError::new(vec![kind], &next)
        }
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

    fn next_is(&self, kind: TokenKind) -> bool {
        match self.peek() {
            Some(token) => token.kind == kind,
            None => false,
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, SyntaxError> {
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

    fn parse_identifier(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::Symbol)?;
        Ok(Expression::Identifier(IdentifierNode {
            name: lexeme,
            loc: Location::new(start, end),
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
                    let property_name = self.parse_identifer_node()?;
                    Expression::PropertyAccess(PropertyAccessNode {
                        loc: Location::new(start, property_name.loc.end),
                        property_name,
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

    fn parse_object(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut properties = vec![];

        loop {
            // If the first iteration returns an empty object
            if self.next_is(TokenKind::ClosedCurlyBrace) {
                break;
            }

            let identifier = self.parse_identifer_node()?;
            self.assert_next(TokenKind::Colon)?;
            let value = self.parse_expression()?;
            properties.push(PropertyNode {
                loc: Location::new(identifier.loc.start, value.loc().end),
                identifier,
                value: value.boxed(),
            });

            if self.next_is(TokenKind::Comma) {
                self.next()?;
            } else {
                break;
            }
        }

        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression::Object(ObjectNode {
            loc: Location::new(start, end),
            properties,
        }))
    }

    fn parse_leaf_expression(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.peek();

        let Some(next) = next else {
            return Ok(self.empty());
        };

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::OpenSquareBracket => self.parse_array(),
            TokenKind::OpenCurlyBrace => self.parse_object(),
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

        Ok(Expression::ForLoop(ForLoopNode {
            loc: Location::new(start, Pos::start()),
            targets,
            iterable,
            body,
        }))
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
        let mut alternate = None;

        if self.next_is(TokenKind::ElseKeyword) {
            alternate = Some(self.parse_else()?);
        }

        Ok(Expression::If(IfNode {
            loc: Location::new(start, consequent.loc().end),
            predicate: predicate.boxed(),
            consequent: consequent.boxed(),
            alternate: Box::new(alternate),
        }))
    }

    fn parse_else(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::ElseKeyword)?;

        let consequent;

        if self.next_is(TokenKind::IfKeyword) {
            consequent = self.parse_if()?;
        } else {
            consequent = self.parse_block()?;
        }

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
        let Token { lexeme, end, .. } = self.assert_next(TokenKind::Symbol)?;
        self.assert_next(TokenKind::Equals)?;
        let right = self.parse_expression()?.boxed();

        Ok(Expression::LetBinding(LetBindingNode {
            name: lexeme,
            right,
            loc: Location::new(start, end),
        }))
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

        Ok(Expression::Fn(FnNode {
            loc: Location::new(start, block.loc().end),
            name: name.lexeme,
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
