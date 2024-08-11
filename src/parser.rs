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
        right: Box<Expression>,
    },

    Int {
        value: i64,
    },

    BinaryAdd {
        left: Box<Expression>,
        right: Box<Expression>,
    },

    Program {
        body: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum ParserError {
    InvalidInteger {
        lexeme: String,
    },

    UnexpectedEOF,

    EmptyProgram,

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
                .filter(|t| t.kind != TokenKind::WhiteSpace && t.kind != TokenKind::Newline)
                .collect::<VecDeque<Token>>(),
        }
    }

    fn next(&mut self) -> Result<Token, ParserError> {
        self.tokens.pop_front().ok_or(ParserError::UnexpectedEOF)
    }

    pub fn parse_program(&mut self) -> Result<Expression, ParserError> {
        let mut expressions = vec![];
        loop {
            let expression = self.parse_expression()?;

            println!("{:?}", &expression.kind);

            expressions.push(expression);

            if self.next_is(TokenKind::SemiColon) {
                self.next()?;
            } else {
                break;
            }
        }

        Ok(Expression {
            start: expressions.first().ok_or(ParserError::EmptyProgram)?.start,
            end: expressions.last().ok_or(ParserError::EmptyProgram)?.end,
            kind: ExpressionKind::Program { body: expressions },
        })
    }

    fn assert_next(&mut self, kind: TokenKind) -> Result<Token, ParserError> {
        let next = self.next()?;
        if next.kind == kind {
            Ok(next)
        } else {
            ParserError::new(vec![kind], &next)
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

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expression = self.parse_leaf_expression()?;

        let Some(next) = self.peek() else {
            return Ok(expression);
        };

        match next.kind {
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
        let next = self.peek().ok_or(ParserError::UnexpectedEOF)?;

        match next.kind {
            TokenKind::FnKeyword => self.parse_fn(),
            TokenKind::IntegerLiteral => self.parse_integer(),
            TokenKind::LetKeyword => self.parse_let(),
            TokenKind::SemiColon => {
                self.next()?;
                self.parse_expression()
            }
            TokenKind::Symbol => {
                let Some(token) = self.peek_at_offset(1) else {
                    return self.parse_identifier();
                };

                if token.kind == TokenKind::OpenParen {
                    return self.parse_invokation();
                };

                self.parse_identifier()
            }
            _ => ParserError::new(
                vec![
                    TokenKind::LetKeyword,
                    TokenKind::FnKeyword,
                    TokenKind::IntegerLiteral,
                    TokenKind::Symbol,
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
        self.assert_next(TokenKind::EqualsOperator)?;
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

    fn parse_parameters(&mut self) -> Result<Vec<String>, ParserError> {
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

    fn parse_fn(&mut self) -> Result<Expression, ParserError> {
        let Token { start, .. } = self.assert_next(TokenKind::FnKeyword)?;
        let name = self.assert_next(TokenKind::Symbol)?;
        let parameters = self.parse_parameters()?;

        self.assert_next(TokenKind::OpenCurlyBrace)?;
        let mut body = vec![];

        loop {
            if self.next_is(TokenKind::ClosedCurlyBrace) {
                break;
            }
            body.push(self.parse_expression()?);

            if self.next_is(TokenKind::ClosedCurlyBrace) {
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

    fn parse(str: &str) -> Expression {
        let tokens = Lexer::new(str).collect::<Vec<_>>();
        Parser::new(tokens).parse_program().expect("Should work")
    }

    #[test]
    fn parsing() {
        let ast = parse(
            "
let a = 3;
let b = 4;
fn test() {
a + b
}",
        );
        assert!(matches!(ast.kind, ExpressionKind::Program { .. }));
        let ast = parse("let a = 3; fn t() { 3 + 4 }; t();");
        assert!(matches!(ast.kind, ExpressionKind::Program { .. }));
        let ast = parse("let a = 4; let b = a + a; fn test() { b + b }");
        assert!(matches!(ast.kind, ExpressionKind::Program { .. }));
    }

    #[test]
    fn only_symbol() {
        let ast = parse("a");
        assert!(matches!(ast.kind, ExpressionKind::Program { .. }));
    }
}
