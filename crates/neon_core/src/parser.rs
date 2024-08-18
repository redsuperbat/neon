use std::collections::VecDeque;

use crate::lexer::{Pos, Token, TokenKind};

pub struct Parser {
    tokens: VecDeque<Token>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub start: Pos,
    pub end: Pos,
    pub kind: ExpressionKind,
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
        Parser {
            tokens: tokens
                .into_iter()
                .filter(|t| t.kind != TokenKind::WhiteSpace && t.kind != TokenKind::Newline)
                .collect::<VecDeque<Token>>(),
        }
    }

    fn next(&mut self) -> Result<Token, SyntaxError> {
        self.tokens.pop_front().ok_or(SyntaxError {
            start: Pos::empty(),
            end: Pos::empty(),
            kind: SyntaxErrorKind::UnexpectedEndOfFile,
        })
    }

    pub fn parse_block(&mut self) -> Result<Expression, SyntaxError> {
        let Token { start, end, .. } = self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut body = vec![];
        let return_val;

        loop {
            let expression = self.parse_expression()?;

            if self.next_is(TokenKind::ClosedCurlyBrace) {
                return_val = Some(expression);
                break;
            } else {
                body.push(expression);
                self.assert_next(TokenKind::SemiColon)?;
            }
        }

        let Some(return_val) = return_val else {
            return Err(SyntaxError {
                kind: SyntaxErrorKind::UnexpectedEndOfBlock,
                start,
                end: body.last().map(|t| t.end.clone()).unwrap_or(end),
            });
        };

        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression {
            start,
            end,
            kind: ExpressionKind::Block {
                return_val: Box::new(return_val),
                body,
            },
        })
    }

    pub fn parse_program(&mut self) -> Result<Expression, SyntaxError> {
        let mut expressions = vec![];
        let return_val;

        loop {
            let expression = self.parse_expression()?;

            if self.peek().is_none() {
                return_val = Some(expression);
                break;
            } else {
                expressions.push(expression);
                self.assert_next(TokenKind::SemiColon)?;
            }
        }

        let Some(return_val) = return_val else {
            return Err(SyntaxError {
                kind: SyntaxErrorKind::UnexpectedEndOfProgram,
                start: expressions
                    .first()
                    .map(|t| t.start.clone())
                    .unwrap_or(Pos::empty()),
                end: expressions
                    .last()
                    .map(|t| t.end.clone())
                    .unwrap_or(Pos::empty()),
            });
        };

        Ok(Expression {
            start: expressions.first().unwrap_or(&return_val).start.clone(),
            end: return_val.end.clone(),
            kind: ExpressionKind::Block {
                body: expressions,
                return_val: Box::new(return_val),
            },
        })
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

    fn is_at_end(&self) -> bool {
        self.peek().is_none()
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
            TokenKind::PlusOperator => {
                self.assert_next(TokenKind::PlusOperator)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Add {
                        left: Box::from(expression),
                        right: Box::from(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
                    },
                };
                Ok(expression)
            }
            TokenKind::MinusOperator => {
                self.assert_next(TokenKind::MinusOperator)?;
                let right = self.parse_expression()?;
                expression = Expression {
                    start: expression.start,
                    end: right.end,
                    kind: ExpressionKind::Sub {
                        left: Box::from(expression),
                        right: Box::new(right),
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
                        left: Box::from(expression),
                        right: Box::new(right),
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
                            left: Box::from(expression),
                            right: Box::new(right),
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
                        callee: Box::from(expression),
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
            start: Pos::empty(),
            end: Pos::empty(),
        })?;

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::FalseKeyword => self.parse_false_keyword(),
            TokenKind::TrueKeyword => self.parse_true_keyword(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::StringLiteral => self.parse_string(),
            TokenKind::IfKeyword => self.parse_if(),
            TokenKind::LetKeyword => self.parse_let(),
            TokenKind::Symbol => self.parse_identifier(),
            TokenKind::ForwardSlash => self.parse_comment(),
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

    fn parse_comment(&mut self) -> Result<Expression, SyntaxError> {
        self.assert_next(TokenKind::ForwardSlash)?;
        while !self.is_at_end() && !self.next_is(TokenKind::ForwardSlash) {
            self.next()?;
        }
        self.assert_next(TokenKind::ForwardSlash)?;
        self.parse_expression()
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
                predicate: Box::new(predicate),
                consequent: Box::from(consequent),
                alternate: Box::from(alternate),
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
                consequent: Box::new(consequent),
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
        let Token { end, lexeme, .. } = self.assert_next(TokenKind::Symbol)?;
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
                body: Box::from(block),
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
        let ast = parse("let a = 3; let b = 4; fn test() { a + b }");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("let a = 3; fn t() { 3 + 4 }; t()");
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
        let ast = parse("let a = 4; let b = a + a; fn test() { b + b }");
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
        let ast = parse(
            "let a = 3+3; / My comment /
a + a",
        );
        assert!(matches!(ast.kind, ExpressionKind::Block { .. }));
    }
}
