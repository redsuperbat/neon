use std::collections::VecDeque;

use crate::lexer::{Pos, Token, TokenKind};

pub struct Parser {
    tokens: VecDeque<Token>,
}

#[derive(Debug)]
pub struct Expression {
    start: Pos,
    end: Pos,
    kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Fn {
        name: String,
        parameters: Vec<Expression>,
        body: Box<Expression>,
    },

    Identifier {
        name: String,
    },

    LetBinding {
        name: String,
    },

    Int {
        value: i64,
    },

    BinaryAdd {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum ParserError {
    InvalidInteger {
        lexeme: String,
    },

    EmptyExpression {
        start: Pos,
        end: Pos,
    },

    UnexpectedEOF,

    UnexpectedToken {
        expected: Vec<TokenKind>,
        found: Token,
    },
}

impl ParserError {
    fn new<R>(expected: Vec<TokenKind>, found: &Token) -> Result<R, ParserError> {
        Err(ParserError::UnexpectedToken {
            expected,
            found: found.clone(),
        })
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens
                .into_iter()
                // remove whitespace since it does not do anything atm
                .filter(|t| t.kind != TokenKind::WhiteSpace)
                .collect::<VecDeque<Token>>(),
        }
    }

    fn next(&mut self) -> Result<Token, ParserError> {
        self.tokens.pop_front().ok_or(ParserError::UnexpectedEOF)
    }

    fn assert_next(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let next = self.next()?;
        if next.kind == kind {
            Ok(next)
        } else {
            ParserError::new(vec![kind], &next)
        }
    }

    fn peek(&self) -> Result<&Token, ParserError> {
        self.tokens.get(0).ok_or(ParserError::UnexpectedEOF)
    }

    fn assert_peek(&self, kind: TokenKind) -> Result<&Token, ParserError> {
        let next = self.peek()?;
        if next.kind == kind {
            Ok(next)
        } else {
            ParserError::new(vec![kind], next)
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expression = self.parse_leaf_expression()?;
        let next = self.peek()?;

        match next.kind {
            TokenKind::EqualsOperator => {
                self.assert_next(TokenKind::EqualsOperator)?;
                expression = Expression {
                    start: expression.start,
                    end: expression.end,
                    kind: ExpressionKind::Assignment {
                        left: Box::from(expression),
                        right: Box::from(self.parse_expression()?),
                    },
                };
                return Ok(expression);
            }
            TokenKind::PlusOperator => {
                self.assert_next(TokenKind::PlusOperator)?;
                expression = Expression {
                    start: expression.start,
                    end: expression.end,
                    kind: ExpressionKind::BinaryAdd {
                        left: Box::from(expression),
                        right: Box::from(self.parse_expression()?),
                    },
                };
                return Ok(expression);
            }
            _ => Ok(expression),
        }
    }

    fn parse_leaf_expression(&mut self) -> Result<Expression, ParserError> {
        let next = self.peek()?;

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::LetKeyword => self.parse_let(),
            TokenKind::Newline => {
                self.next()?;
                self.parse_expression()
            }
            TokenKind::ClosedCurlyBrace => Err(ParserError::EmptyExpression {
                start: next.start,
                end: next.end,
            }),
            _ => ParserError::new(
                vec![
                    TokenKind::LetKeyword,
                    TokenKind::FnKeyword,
                    TokenKind::IntegerLiteral,
                ],
                next,
            ),
        }
    }

    fn parse_integer(&mut self) -> Result<Expression, ParserError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::IntegerLiteral)?;

        let value = lexeme
            .parse::<i64>()
            .map_err(|_| ParserError::InvalidInteger { lexeme })?;

        Ok(Expression {
            start,
            end,
            kind: ExpressionKind::Int { value },
        })
    }

    fn parse_let(&mut self) -> Result<Expression, ParserError> {
        let Token { start, .. } = self.assert_next(TokenKind::LetKeyword)?;
        let Token { end, lexeme, .. } = self.assert_next(TokenKind::Symbol)?;
        Ok(Expression {
            kind: ExpressionKind::LetBinding { name: lexeme },
            start,
            end,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Expression>, ParserError> {
        self.assert_next(TokenKind::OpenParen)?;
        let mut params = vec![];

        loop {
            if self.peek()?.kind == TokenKind::ClosedParen {
                break;
            } else {
                self.assert_peek(TokenKind::Comma)?;
            }

            let name = self.assert_next(TokenKind::Symbol)?;
            params.push(Expression {
                start: name.start,
                end: name.end,
                kind: ExpressionKind::Identifier { name: name.lexeme },
            });
        }

        self.assert_next(TokenKind::ClosedParen)?;
        Ok(params)
    }

    fn parse_fn(&mut self) -> Result<Expression, ParserError> {
        let Token { start, .. } = self.assert_next(TokenKind::FnKeyword)?;
        let name = self.assert_next(TokenKind::Symbol)?;
        let parameters = self.parse_parameters()?;

        self.assert_next(TokenKind::OpenCurlyBrace)?;
        let body = self.parse_expression()?;

        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression {
            start,
            end,
            kind: ExpressionKind::Fn {
                name: name.lexeme,
                parameters,
                body: Box::from(body),
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn parsing() {
        let code = String::from(
            "fn test() {
3
}",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();

        let ast = Parser::new(tokens).parse_expression();
        println!("{:?}", ast);
    }
}
