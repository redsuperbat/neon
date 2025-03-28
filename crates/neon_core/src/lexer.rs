use crate::location::{Location, Pos};

#[derive(Debug)]
pub struct Lexer {
    text: Vec<char>,
    col: usize,
    line: usize,
    offset: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Pos,
    pub end: Pos,
    pub lexeme: String,
}

impl Into<Location> for &Token {
    fn into(self) -> Location {
        Location::new(self.start, self.end)
    }
}

impl Into<Location> for Token {
    fn into(self) -> Location {
        (&self).into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    AsKeyword,
    BreakKeyword,
    ContinueKeyword,
    ElseKeyword,
    EnumKeyword,
    FalseKeyword,
    FnKeyword,
    ForKeyword,
    FromKeyword,
    IfKeyword,
    ImportKeyword,
    InKeyword,
    LetKeyword,
    LoopKeyword,
    MatchKeyword,
    PubKeyword,
    ReturnKeyword,
    SelfKeyword,
    StructKeyword,
    TrueKeyword,
    TypeKeyword,
    UseKeyword,
    ValKeyword,
    VarKeyword,
    WhileKeyword,

    IntegerLiteral,
    StringLiteral,

    Symbol,

    Ampersand,           // &
    Asterisk,            // *
    Bang,                // !
    SingleQuote,         // '
    ClosedAngleBracket,  // >
    ClosedCurlyBrace,    // }
    ClosedParen,         // )
    ClosedSquareBracket, // ]
    Colon,               // :
    Comma,               // ,
    DollarSign,          // $
    Dot,                 // .
    Equals,              // =
    ForwardSlash,        // /
    Minus,               // -
    Newline,             // \n
    OpenAngleBracket,    // <
    OpenCurlyBrace,      // {
    OpenParen,           // (
    OpenSquareBracket,   // ]
    Percentage,          // %
    Pipe,                // |
    Plus,                // +
    SemiColon,           // ;
    WhiteSpace,          // ' '

    Unknown(String),
}

impl ToString for TokenKind {
    fn to_string(&self) -> String {
        let s = match self {
            TokenKind::FnKeyword => "fn",
            TokenKind::LetKeyword => "let",
            TokenKind::IfKeyword => "if",
            TokenKind::ElseKeyword => "else",
            TokenKind::TrueKeyword => "true",
            TokenKind::FalseKeyword => "false",
            TokenKind::ForKeyword => "for",
            TokenKind::InKeyword => "in",
            TokenKind::LoopKeyword => "loop",
            TokenKind::WhileKeyword => "while",
            TokenKind::StructKeyword => "struct",
            TokenKind::SelfKeyword => "self",
            TokenKind::PubKeyword => "pub",
            TokenKind::TypeKeyword => "type",
            TokenKind::MatchKeyword => "match",
            TokenKind::BreakKeyword => "break",
            TokenKind::ContinueKeyword => "continue",
            TokenKind::UseKeyword => "use",
            TokenKind::AsKeyword => "as",
            TokenKind::ImportKeyword => "import",
            TokenKind::FromKeyword => "from",
            TokenKind::ReturnKeyword => "return",
            TokenKind::EnumKeyword => "enum",
            TokenKind::VarKeyword => "var",
            TokenKind::ValKeyword => "val",

            TokenKind::OpenAngleBracket => "<",
            TokenKind::ClosedAngleBracket => ">",
            TokenKind::Minus => "-",
            TokenKind::ForwardSlash => "/",
            TokenKind::Plus => "+",
            TokenKind::Ampersand => "&",
            TokenKind::Pipe => "|",
            TokenKind::Asterisk => "*",

            TokenKind::SemiColon => ";",
            TokenKind::Colon => ":",
            TokenKind::Percentage => "%",
            TokenKind::Equals => "=",
            TokenKind::Bang => "!",
            TokenKind::OpenCurlyBrace => "{",
            TokenKind::ClosedCurlyBrace => "}",
            TokenKind::OpenSquareBracket => "[",
            TokenKind::ClosedSquareBracket => "]",
            TokenKind::OpenParen => "(",
            TokenKind::ClosedParen => ")",
            TokenKind::Comma => ",",
            TokenKind::Newline => "newline",
            TokenKind::IntegerLiteral => "integer",
            TokenKind::StringLiteral => "string",
            TokenKind::Symbol => "symbol",
            TokenKind::WhiteSpace => "whitespace",
            TokenKind::Unknown(lexeme) => lexeme,
            TokenKind::Dot => ".",
            TokenKind::DollarSign => "$",
            TokenKind::SingleQuote => "'",
        };
        s.to_string()
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Lexer {
    pub fn new<T: AsRef<str>>(text: T) -> Lexer {
        return Lexer {
            text: text.as_ref().chars().collect(),
            col: 1,
            line: 1,
            offset: 0,
        };
    }

    pub fn vec(&mut self) -> Vec<Token> {
        self.collect::<Vec<Token>>()
    }

    fn pos(&self) -> Pos {
        Pos(self.line, self.col)
    }

    fn next_token(&mut self) -> Option<Token> {
        let next_char = self.peek()?;

        match next_char {
            '=' => self.single_char(TokenKind::Equals),
            '!' => self.single_char(TokenKind::Bang),
            '\'' => self.single_char(TokenKind::SingleQuote),

            '<' => self.single_char(TokenKind::OpenAngleBracket),
            '>' => self.single_char(TokenKind::ClosedAngleBracket),
            '/' => self.single_char(TokenKind::ForwardSlash),
            '+' => self.single_char(TokenKind::Plus),
            '-' => self.single_char(TokenKind::Minus),
            '*' => self.single_char(TokenKind::Asterisk),
            ']' => self.single_char(TokenKind::ClosedSquareBracket),
            '[' => self.single_char(TokenKind::OpenSquareBracket),
            '.' => self.single_char(TokenKind::Dot),

            ';' => self.single_char(TokenKind::SemiColon),
            ':' => self.single_char(TokenKind::Colon),

            '{' => self.single_char(TokenKind::OpenCurlyBrace),
            '}' => self.single_char(TokenKind::ClosedCurlyBrace),

            '(' => self.single_char(TokenKind::OpenParen),
            ')' => self.single_char(TokenKind::ClosedParen),
            ',' => self.single_char(TokenKind::Comma),
            '%' => self.single_char(TokenKind::Percentage),
            '&' => self.single_char(TokenKind::Ampersand),
            '|' => self.single_char(TokenKind::Pipe),
            '"' => self.string_literal(),

            ' ' => self.whitespace(),

            '\n' => self.single_char(TokenKind::Newline),

            'a'..='z' | 'A'..='Z' => self.symbol_or_keyword(),
            '0'..='9' => self.integer_literal(),

            _ => self.single_char(TokenKind::Unknown(next_char.to_string())),
        }
    }

    fn next(&mut self) -> Option<char> {
        let char = self.peek()?;

        self.col += 1;

        if char == '\n' {
            self.col = 1;
            self.line += 1;
        }
        self.offset += 1;

        Some(char)
    }

    fn string_literal(&mut self) -> Option<Token> {
        let start = self.pos();
        self.next()?; // remove first "
        let mut lexeme = String::new();

        while self.peek().is_some() && self.peek()? != '"' {
            lexeme.push(self.next()?);
        }
        self.next()?; // remove last "

        let end = self.pos();
        Some(Token {
            end,
            lexeme,
            start,
            kind: TokenKind::StringLiteral,
        })
    }

    fn integer_literal(&mut self) -> Option<Token> {
        let start = self.pos();
        let mut lexeme = self.next()?.to_string();

        while self.peek().is_some() && ('0'..='9').contains(&self.peek()?) {
            let next = self.next()?.to_string();
            lexeme += &next
        }

        let end = self.pos();
        Some(Token {
            start,
            end,
            lexeme,
            kind: TokenKind::IntegerLiteral,
        })
    }

    fn peek(&self) -> Option<char> {
        self.text.get(self.offset).map(|c| *c)
    }

    fn whitespace(&mut self) -> Option<Token> {
        let start = self.pos();
        let mut lexeme = String::new();
        while ' ' == self.peek()? {
            lexeme += &self.next()?.to_string();
        }
        let end = self.pos();
        Some(Token {
            end,
            lexeme,
            kind: TokenKind::WhiteSpace,
            start,
        })
    }

    fn single_char(&mut self, kind: TokenKind) -> Option<Token> {
        let start = self.pos();
        let lexeme = String::from(self.next()?);
        let end = self.pos();
        Some(Token {
            kind,
            end,
            lexeme,
            start,
        })
    }

    fn symbol_or_keyword(&mut self) -> Option<Token> {
        let start = self.pos();
        let mut lexeme = String::new();

        loop {
            let Some(char) = self.peek() else {
                break;
            };

            match char {
                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                    lexeme += &self.next()?.to_string();
                }
                _ => break,
            }
        }

        let end = self.pos();

        let kind = match lexeme.as_str() {
            "let" => TokenKind::LetKeyword,
            "fn" => TokenKind::FnKeyword,
            "if" => TokenKind::IfKeyword,
            "else" => TokenKind::ElseKeyword,
            "true" => TokenKind::TrueKeyword,
            "false" => TokenKind::FalseKeyword,
            "for" => TokenKind::ForKeyword,
            "in" => TokenKind::InKeyword,
            "loop" => TokenKind::LoopKeyword,
            "while" => TokenKind::WhileKeyword,
            "struct" => TokenKind::StructKeyword,
            "self" => TokenKind::SelfKeyword,
            "pub" => TokenKind::PubKeyword,
            "type" => TokenKind::TypeKeyword,
            "match" => TokenKind::MatchKeyword,
            "continue" => TokenKind::ContinueKeyword,
            "break" => TokenKind::BreakKeyword,
            "as" => TokenKind::AsKeyword,
            "use" => TokenKind::UseKeyword,
            "import" => TokenKind::ImportKeyword,
            "from" => TokenKind::FromKeyword,
            "return" => TokenKind::ReturnKeyword,
            "enum" => TokenKind::EnumKeyword,
            "var" => TokenKind::VarKeyword,
            "val" => TokenKind::ValKeyword,

            _ => TokenKind::Symbol,
        };
        Some(Token {
            end,
            start,
            kind,
            lexeme,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(code: &str) -> Vec<Token> {
        Lexer::new(code).collect::<Vec<_>>()
    }

    fn assert_tokens(code: &str, tokens: Vec<TokenKind>) {
        let lexed = Lexer::new(code).collect::<Vec<_>>();
        for (i, expected) in tokens.iter().enumerate() {
            let got = lexed
                .get(i)
                .expect(&format!("Should find token {:?}", expected));
            assert_eq!(got.kind, *expected)
        }
    }

    fn assert_significant_tokens(code: &str, tokens: Vec<TokenKind>) {
        let lexed = Lexer::new(code)
            .filter(|t| t.kind != TokenKind::WhiteSpace)
            .filter(|t| t.kind != TokenKind::Newline)
            .collect::<Vec<_>>();
        for (i, expected) in tokens.iter().enumerate() {
            let got = lexed
                .get(i)
                .expect(&format!("Should find token {:?}", expected));
            assert_eq!(got.kind, *expected)
        }
    }

    #[test]
    fn lexing() {
        let tokens = lex("fn test() {3}");
        assert_eq!(tokens.len(), 9);
        let tokens = lex("a");
        assert_eq!(tokens.len(), 1);
        let tokens = lex("fn test() {3}; test(); let a = 123");
        assert_eq!(tokens.len(), 23);
        assert_tokens(
            "if let else fn",
            vec![
                TokenKind::IfKeyword,
                TokenKind::WhiteSpace,
                TokenKind::LetKeyword,
                TokenKind::WhiteSpace,
                TokenKind::ElseKeyword,
                TokenKind::WhiteSpace,
                TokenKind::FnKeyword,
            ],
        );

        assert_tokens(
            "15 == 9",
            vec![
                TokenKind::IntegerLiteral,
                TokenKind::WhiteSpace,
                TokenKind::Equals,
                TokenKind::Equals,
                TokenKind::WhiteSpace,
                TokenKind::IntegerLiteral,
            ],
        );

        assert_significant_tokens(
            "hello() + hello()",
            vec![
                TokenKind::Symbol,
                TokenKind::OpenParen,
                TokenKind::ClosedParen,
                TokenKind::Plus,
                TokenKind::Symbol,
                TokenKind::OpenParen,
                TokenKind::ClosedParen,
            ],
        );
        assert_significant_tokens(
            "\"hello()\" + hello()",
            vec![
                TokenKind::StringLiteral,
                TokenKind::Plus,
                TokenKind::Symbol,
                TokenKind::OpenParen,
                TokenKind::ClosedParen,
            ],
        );
        assert_significant_tokens(
            "<>",
            vec![TokenKind::OpenAngleBracket, TokenKind::ClosedAngleBracket],
        );
    }
}
