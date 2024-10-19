use neon_core::{
    lexer::{Token, TokenKind},
    location::Pos,
};
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
    PrimitiveType,
    Builtin,
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
            SemanticTokenKind::Builtin => "builtin",
            SemanticTokenKind::PrimitiveType => "primitive",
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
        self.peek_at_offset(0)
    }

    fn next_pair_is(&self, tokens: (TokenKind, TokenKind)) -> bool {
        match self.peek() {
            Some(a) => match self.peek_at_offset(1) {
                Some(b) => a.kind == tokens.0 && b.kind == tokens.1,
                None => false,
            },
            None => false,
        }
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

            TokenKind::LetKeyword
            | TokenKind::IfKeyword
            | TokenKind::ElseKeyword
            | TokenKind::ForKeyword
            | TokenKind::WhileKeyword
            | TokenKind::LoopKeyword
            | TokenKind::SelfKeyword
            | TokenKind::StructKeyword
            | TokenKind::PubKeyword
            | TokenKind::TypeKeyword
            | TokenKind::InKeyword => self.single(SemanticTokenKind::Keyword),

            TokenKind::Ampersand
            | TokenKind::Bang
            | TokenKind::Equals
            | TokenKind::Pipe
            | TokenKind::Plus
            | TokenKind::Minus
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
        // For /* comments
        if self.next_is(TokenKind::Asterisk) {
            while !self.next_pair_is((TokenKind::Asterisk, TokenKind::ForwardSlash)) {
                self.next()?;
            }
            self.next()?;
        }

        // For //  comments
        if self.next_is(TokenKind::ForwardSlash) {
            while !self.next_is(TokenKind::Newline) {
                self.next()?;
            }
        }

        let Token { end, .. } = self.next()?;

        Some(SemanticToken {
            start,
            end,
            kind: SemanticTokenKind::Comment,
        })
    }

    fn analyze_symbol(&mut self) -> Option<SemanticToken> {
        let symbol = self.peek()?;
        let is_fn = self.is_at_offset(1, TokenKind::OpenParen);

        if !is_fn {
            return match symbol.lexeme.as_str() {
                "int" | "string" | "bool" | "unit" => self.single(SemanticTokenKind::PrimitiveType),
                _ => self.single(SemanticTokenKind::VariableName),
            };
        }

        match symbol.lexeme.as_str() {
            "print" => self.single(SemanticTokenKind::Builtin),
            _ => self.single(SemanticTokenKind::FnCall),
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
        analyze("let a  = 3");
    }
}
