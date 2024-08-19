use neon_core::lexer::{Pos, Token, TokenKind};
use std::collections::VecDeque;

#[derive(Debug)]
pub struct SemanticToken {
    pub kind: SemanticTokenKind,
    pub start: Pos,
    pub end: Pos,
}

#[derive(Debug)]
pub enum SemanticTokenKind {
    FnName,
    FnCall,
    Bool,
    Comment,
    VariableName,
    String,
    Int,
    Keyword,
    Operator,
}

impl ToString for SemanticTokenKind {
    fn to_string(&self) -> String {
        let str = match self {
            SemanticTokenKind::FnName => "fn-name",
            SemanticTokenKind::FnCall => "fn-call",
            SemanticTokenKind::Bool => "bool",
            SemanticTokenKind::Comment => "comment",
            SemanticTokenKind::VariableName => "variable",
            SemanticTokenKind::String => "string",
            SemanticTokenKind::Int => "int",
            SemanticTokenKind::Keyword => "keyword",
            SemanticTokenKind::Operator => "operator",
        };
        str.to_string()
    }
}

#[derive(Debug)]
enum LexicalScope {
    Default,
    Fn,
}

pub struct SemanticAnalyzer {
    tokens: VecDeque<Token>,
    lexical_scope: VecDeque<LexicalScope>,
}

impl Iterator for SemanticAnalyzer {
    type Item = SemanticToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl SemanticAnalyzer {
    pub fn new(tokens: Vec<Token>) -> SemanticAnalyzer {
        SemanticAnalyzer {
            tokens: VecDeque::from(tokens),
            lexical_scope: VecDeque::from(vec![LexicalScope::Default]),
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn is_at_end(&self) -> bool {
        self.peek().is_none()
    }

    fn is_at_offset(&self, offset: usize, token: TokenKind) -> bool {
        match self.peek_at_offset(offset) {
            Some(next) => next.kind == token,
            None => false,
        }
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

    fn next_token(&mut self) -> Option<SemanticToken> {
        let scope = self.lexical_scope.front()?;
        println!("{:?}", scope);
        match scope {
            LexicalScope::Default => self.default_scope_token(),
            LexicalScope::Fn => self.fn_scope_token(),
        }
    }

    fn default_scope_token(&mut self) -> Option<SemanticToken> {
        let next = self.peek()?;
        match next.kind {
            TokenKind::FnKeyword => {
                self.lexical_scope.push_front(LexicalScope::Fn);
                self.single(SemanticTokenKind::Keyword)
            }
            TokenKind::LetKeyword | TokenKind::IfKeyword | TokenKind::ElseKeyword => {
                self.single(SemanticTokenKind::Keyword)
            }

            TokenKind::Ampersand
            | TokenKind::Bang
            | TokenKind::Equals
            | TokenKind::Pipe
            | TokenKind::Percentage
            | TokenKind::OpenAngleBracket
            | TokenKind::ClosedAngleBracket => self.single(SemanticTokenKind::Operator),

            TokenKind::TrueKeyword | TokenKind::FalseKeyword => {
                self.single(SemanticTokenKind::Bool)
            }
            TokenKind::IntegerLiteral => self.single(SemanticTokenKind::Int),
            TokenKind::StringLiteral => self.single(SemanticTokenKind::String),

            TokenKind::ForwardSlash => self.analyze_forward_slash(),
            TokenKind::Symbol => self.analyze_symbol(),
            _ => {
                self.next()?;
                self.next_token()
            }
        }
    }

    fn fn_scope_token(&mut self) -> Option<SemanticToken> {
        let next = self.peek()?;
        match next.kind {
            TokenKind::Symbol => {
                self.lexical_scope.pop_front();
                self.single(SemanticTokenKind::FnName)
            }
            _ => self.default_scope_token(),
        }
    }

    fn single(&mut self, kind: SemanticTokenKind) -> Option<SemanticToken> {
        let Token { start, end, .. } = self.next()?;
        Some(SemanticToken { kind, end, start })
    }

    fn analyze_forward_slash(&mut self) -> Option<SemanticToken> {
        let Token { start, .. } = self.next()?;
        while !self.is_at_end() && !self.next_is(TokenKind::ForwardSlash) {
            self.next()?;
        }
        let Token { end, .. } = self.next()?;

        Some(SemanticToken {
            start,
            end,
            kind: SemanticTokenKind::Comment,
        })
    }

    fn analyze_symbol(&mut self) -> Option<SemanticToken> {
        if self.is_at_offset(1, TokenKind::OpenParen) {
            self.single(SemanticTokenKind::FnCall)
        } else {
            self.single(SemanticTokenKind::VariableName)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use neon_core::lexer::Lexer;

    fn analyze(str: &str) -> Vec<SemanticToken> {
        let tokens = Lexer::new(str).collect::<Vec<_>>();
        SemanticAnalyzer::new(tokens).collect()
    }

    #[test]
    fn analyzing() {
        let tokens = analyze("let a  = 3");
        println!("{:?}", tokens);
    }
}
