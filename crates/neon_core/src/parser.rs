use std::collections::VecDeque;

use crate::lexer::{Pos, Token, TokenKind};

struct Location {
    start: Pos,
    end: Pos,
}

impl Location {
    fn new(start: Pos, end: Pos) -> Location {
        Location { end, start }
    }
}

pub struct Parser {
    tokens: VecDeque<Token>,
    last_location: Location,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub start: Pos,
    pub end: Pos,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn kind(kind: ExpressionKind) -> Expression {
        Expression {
            kind,
            end: Pos::start(),
            start: Pos::start(),
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

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Fn {
        name: String,
        parameters: Vec<String>,
        body: Box<Expression>,
    },

    Block {
        body: Vec<Expression>,
        return_val: Box<Expression>,
    },

    Identifier {
        name: String,
    },

    Invocation {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },

    LetBinding {
        name: String,
        right: Box<Expression>,
    },

    If {
        predicate: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Option<Expression>>,
    },

    Else {
        consequent: Box<Expression>,
    },

    Empty,

    Int {
        value: i64,
    },

    String {
        value: String,
    },

    Bool {
        value: bool,
    },

    And {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Or {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Add {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Sub {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Lt {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Gt {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Modulus {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Ne {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Eq {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Builtin {
        kind: BuiltinExpressionKind,
        arguments: Vec<String>,
    },
}

#[derive(Debug)]
pub struct SyntaxError {
    pub kind: SyntaxErrorKind,
    pub start: Pos,
    pub end: Pos,
}

#[derive(Debug)]
pub enum SyntaxErrorKind {
    UnexpectedEndOfBlock,

    UnexpectedEndOfProgram,

    UnexpectedEndOfFile,

    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: TokenKind,
    },
}

impl ToString for SyntaxErrorKind {
    fn to_string(&self) -> String {
        format!("{:?}", self)
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
        let tokens = tokens
            .into_iter()
            .filter(|t| t.kind != TokenKind::WhiteSpace && t.kind != TokenKind::Newline);

        let mut token_queue = VecDeque::new();
        let mut in_comment = false;
        for token in tokens {
            if token.kind == TokenKind::ForwardSlash {
                in_comment = !in_comment;
            } else if !in_comment {
                token_queue.push_back(token);
            }
        }

        Parser {
            tokens: token_queue,
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

    fn is_at_end(&self) -> bool {
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

    fn next_is(&self, kind: TokenKind) -> bool {
        match self.peek() {
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
        let mut expression = self.parse_sub_leaf_expression()?;

        let Some(next) = self.peek() else {
            return Ok(expression);
        };

        match next.kind {
            TokenKind::Plus => {
                self.assert_next(TokenKind::Plus)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Add {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Percentage => {
                self.assert_next(TokenKind::Percentage)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Modulus {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Ampersand => {
                self.assert_next(TokenKind::Ampersand)?;
                self.assert_next(TokenKind::Ampersand)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::And {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Pipe => {
                self.assert_next(TokenKind::Pipe)?;
                self.assert_next(TokenKind::Pipe)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Or {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }

            TokenKind::OpenAngleBracket => {
                self.assert_next(TokenKind::OpenAngleBracket)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Lt {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::ClosedAngleBracket => {
                self.assert_next(TokenKind::ClosedAngleBracket)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Gt {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Minus => {
                self.assert_next(TokenKind::Minus)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Sub {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Bang => {
                self.assert_next(TokenKind::Bang)?;
                self.assert_next(TokenKind::Equals)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Ne {
                        left: expression.boxed(),
                        right: right.boxed(),
                    },
                };
                Ok(expression)
            }
            TokenKind::Equals => {
                if self.at_offet_is(1, TokenKind::Equals) {
                    self.assert_next(TokenKind::Equals)?;
                    self.assert_next(TokenKind::Equals)?;
                    let right = self.parse_expression()?;
                    expression = Expression {
                        start: expression.start,
                        end: right.end,
                        kind: ExpressionKind::Eq {
                            left: expression.boxed(),
                            right: right.boxed(),
                        },
                    };
                    Ok(expression)
                } else {
                    Ok(expression)
                }
            }
            _ => Ok(expression),
        }
    }

    fn parse_identifier(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::Symbol)?;
        Ok(Expression {
            kind: ExpressionKind::Identifier { name: lexeme },
            end,
            start,
        })
    }

    fn parse_sub_leaf_expression(&mut self) -> Result<Expression, SyntaxError> {
        let mut expression = self.parse_leaf_expression()?;

        let Some(next) = self.peek() else {
            return Ok(expression);
        };

        match next.kind {
            TokenKind::OpenParen => {
                expression = Expression {
                    start: expression.start,
                    end: expression.end.clone(),
                    kind: ExpressionKind::Invocation {
                        callee: expression.boxed(),
                        arguments: self.parse_arguments()?,
                    },
                };
                Ok(expression)
            }
            _ => Ok(expression),
        }
    }

    fn parse_leaf_expression(&mut self) -> Result<Expression, SyntaxError> {
        let next = self.peek().ok_or(SyntaxError {
            kind: SyntaxErrorKind::UnexpectedEndOfFile,
            start: Pos::start(),
            end: Pos::start(),
        })?;

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::OpenCurlyBrace => self.parse_block(),
            TokenKind::ClosedCurlyBrace => self.parse_empty(),
            TokenKind::FalseKeyword => self.parse_false_keyword(),
            TokenKind::TrueKeyword => self.parse_true_keyword(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::StringLiteral => self.parse_string(),
            TokenKind::IfKeyword => self.parse_if(),
            TokenKind::LetKeyword => self.parse_let(),
            TokenKind::Symbol => self.parse_identifier(),
            _ => SyntaxError::new(
                vec![
                    TokenKind::FnKeyword,
                    TokenKind::IntegerLiteral,
                    TokenKind::IfKeyword,
                    TokenKind::LetKeyword,
                    TokenKind::Symbol,
                ],
                next,
            ),
        }
    }

    fn parse_block_body(&mut self) -> Result<(Vec<Expression>, Expression), SyntaxError> {
        let mut body = vec![];
        let mut return_val = Expression {
            start: self.last_location.start,
            end: self.last_location.end,
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
            start,
            end,
            kind: ExpressionKind::Block {
                return_val: return_val.boxed(),
                body,
            },
        })
    }

    pub fn parse_program(&mut self) -> Result<Expression, SyntaxError> {
        let (body, return_val) = self.parse_block_body()?;

        Ok(Expression {
            start: body.first().unwrap_or(&return_val).start,
            end: return_val.end,
            kind: ExpressionKind::Block {
                body,
                return_val: return_val.boxed(),
            },
        })
    }

    fn parse_empty(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;
        Ok(Expression {
            kind: ExpressionKind::Empty,
            start,
            end,
        })
    }

    fn parse_string(&mut self) -> Result<Expression, SyntaxError> {
        let Token {
            start, end, lexeme, ..
        } = self.assert_next(TokenKind::StringLiteral)?;
        Ok(Expression {
            kind: ExpressionKind::String { value: lexeme },
            start,
            end,
        })
    }

    fn parse_true_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::TrueKeyword)?;
        Ok(Expression {
            kind: ExpressionKind::Bool { value: true },
            start,
            end,
        })
    }

    fn parse_false_keyword(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::FalseKeyword)?;
        Ok(Expression {
            kind: ExpressionKind::Bool { value: false },
            start,
            end,
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
            start,
            end: consequent.end,
            kind: ExpressionKind::If {
                predicate: predicate.boxed(),
                consequent: consequent.boxed(),
                alternate: Box::new(alternate),
            },
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
            start,
            end: consequent.end,
            kind: ExpressionKind::Else {
                consequent: consequent.boxed(),
            },
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
            start,
            end,
            kind: ExpressionKind::Int { value },
        })
    }

    fn parse_let(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, .. } = self.assert_next(TokenKind::LetKeyword)?;
        let Token { lexeme, end, .. } = self.assert_next(TokenKind::Symbol)?;
        self.assert_next(TokenKind::Equals)?;
        let right = self.parse_expression().map(Box::new)?;

        Ok(Expression {
            kind: ExpressionKind::LetBinding {
                name: lexeme,
                right,
            },
            start,
            end,
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
            start,
            end: block.end.clone(),
            kind: ExpressionKind::Fn {
                name: name.lexeme,
                parameters,
                body: block.boxed(),
            },
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
