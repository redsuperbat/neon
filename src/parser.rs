use std::collections::VecDeque;

use crate::lexer::{Pos, Token, TokenKind};

pub struct Parser {
    tokens: VecDeque<Token>,
}

#[derive(Debug)]
pub struct Expression {
    pub start: Pos,
    pub end: Pos,
    pub kind: ExpressionKind,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Fn {
        name: String,
        parameters: Vec<String>,
        body: Vec<Expression>,
    },

    Identifier {
        name: String,
    },

    Invocation {
        name: String,
        arguments: Vec<Expression>,
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

    pub fn parse_program(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut program = vec![];
        loop {
            program.push(self.parse_expression()?);

            let Ok(token) = self.peek() else {
                break;
            };

            if token.kind != TokenKind::Newline {
                break;
            }
        }
        Ok(program)
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
        self.peek_at_offset(0)
    }

    fn peek_at_offset(&self, i: usize) -> Result<&Token, ParserError> {
        self.tokens.get(i).ok_or(ParserError::UnexpectedEOF)
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expression = self.parse_leaf_expression()?;

        let Some(next) = self.peek().ok() else {
            return Ok(expression);
        };

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

    fn parse_identifier(&mut self) -> Result<Expression, ParserError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::Symbol)?;
        Ok(Expression {
            kind: ExpressionKind::Identifier { name: lexeme },
            end,
            start,
        })
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
            TokenKind::SemiColon => {
                self.next()?;
                self.parse_expression()
            }
            TokenKind::Symbol => {
                if self.peek_at_offset(1)?.kind == TokenKind::OpenParen {
                    self.parse_invokation()
                } else {
                    self.parse_identifier()
                }
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

    fn parse_invokation(&mut self) -> Result<Expression, ParserError> {
        let Token {
            lexeme, start, end, ..
        } = self.assert_next(TokenKind::Symbol)?;

        let mut arguments = vec![];
        self.assert_next(TokenKind::OpenParen)?;

        loop {
            // Handle immediately closed parens
            if self.peek()?.kind == TokenKind::ClosedParen {
                break;
            }

            arguments.push(self.parse_expression()?);

            // Handle function arguments
            if self.peek()?.kind == TokenKind::ClosedParen {
                break;
            } else {
                self.assert_next(TokenKind::Comma)?;
            }
        }

        Ok(Expression {
            start,
            end,
            kind: ExpressionKind::Invocation {
                name: lexeme,
                arguments,
            },
        })
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

    fn parse_parameters(&mut self) -> Result<Vec<String>, ParserError> {
        self.assert_next(TokenKind::OpenParen)?;
        let mut params = vec![];

        loop {
            // Handle immediately closed parens
            if self.peek()?.kind == TokenKind::ClosedParen {
                break;
            }

            let name = self.assert_next(TokenKind::Symbol)?;
            params.push(name.lexeme);

            // Handle function parameters
            if self.peek()?.kind == TokenKind::ClosedParen {
                break;
            } else {
                self.assert_next(TokenKind::Comma)?;
            }
        }

        self.assert_next(TokenKind::ClosedParen)?;
        Ok(params)
    }

    fn parse_fn(&mut self) -> Result<Expression, ParserError> {
        let Token { start, .. } = self.assert_next(TokenKind::FnKeyword)?;
        let name = self.assert_next(TokenKind::Symbol)?;
        let parameters = self.parse_parameters()?;

        self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut body = vec![];

        loop {
            if self.peek()?.kind == TokenKind::ClosedCurlyBrace {
                break;
            }
            body.push(self.parse_expression()?);

            if self.peek()?.kind == TokenKind::ClosedCurlyBrace {
                break;
            } else {
                self.assert_next(TokenKind::Newline)?;
            }
        }

        let Token { end, .. } = self.assert_next(TokenKind::ClosedCurlyBrace)?;

        Ok(Expression {
            start,
            end,
            kind: ExpressionKind::Fn {
                name: name.lexeme,
                parameters,
                body,
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
            "
let a = 3
let b = 4
fn test() {
a + b
}",
        );
        let tokens = Lexer::new(code).collect::<Vec<_>>();

        let ast = Parser::new(tokens).parse_program();
        println!("{:?}", ast);
    }
}
