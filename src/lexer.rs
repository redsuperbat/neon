#[derive(Debug)]
pub struct Lexer {
    text: String,
    col: usize,
    row: usize,
    offset: usize,
}

pub type Pos = (usize, usize);

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: Pos,
    pub end: Pos,
    pub lexeme: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    FnKeyword,  // fn
    LetKeyword, // let

    EqualsOperator, // =
    PlusOperator,   // +

    OpenCurlyBrace,   // {
    ClosedCurlyBrace, // }
    OpenParen,        // (
    ClosedParen,      // )
    Comma,            // ,

    IntegerLiteral, // 5 -3 etc.

    Symbol,     // abc
    Newline,    // \n
    WhiteSpace, // ' '

    Unknown,
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
            text: text.as_ref().to_string(),
            col: 1,
            row: 1,
            offset: 0,
        };
    }

    pub fn vec(&mut self) -> Vec<Token> {
        self.collect::<Vec<Token>>()
    }

    fn get_pos(&self) -> Pos {
        (self.row, self.col)
    }

    fn next_token(&mut self) -> Option<Token> {
        let next_char = self.peek();
        let Some(next_char) = next_char else {
            return Option::None;
        };

        let token = match next_char {
            '=' => self.single_char(TokenKind::EqualsOperator),
            '+' => self.single_char(TokenKind::PlusOperator),
            '{' => self.single_char(TokenKind::OpenCurlyBrace),
            '}' => self.single_char(TokenKind::ClosedCurlyBrace),
            '(' => self.single_char(TokenKind::OpenParen),
            ')' => self.single_char(TokenKind::ClosedParen),
            ',' => self.single_char(TokenKind::Comma),

            ' ' => self.whitespace(),

            '\n' => self.single_char(TokenKind::Newline),

            'A'..='z' => self.symbol_or_keyword()?,
            '0'..='9' => self.integer_literal()?,

            _ => self.single_char(TokenKind::Unknown),
        };
        Some(token)
    }

    fn next(&mut self) -> Option<char> {
        let next = self.peek();
        let Some(char) = next else {
            return Option::None;
        };

        self.col += 1;

        if char == '\n' {
            self.col = 1;
            self.row += 1;
        }
        self.offset += 1;

        Some(char)
    }

    fn integer_literal(&mut self) -> Option<Token> {
        let start = self.get_pos();
        let mut lexeme = String::new();
        while ('0'..='9').contains(&self.peek()?) {
            let next = self.next()?.to_string();
            lexeme += &next
        }
        let end = self.get_pos();
        Some(Token {
            start,
            end,
            lexeme,
            kind: TokenKind::IntegerLiteral,
        })
    }

    fn peek(&self) -> Option<char> {
        self.text.chars().nth(self.offset)
    }

    fn whitespace(&mut self) -> Token {
        let start = self.get_pos();
        let mut lexeme = String::new();
        while ' ' == self.peek().unwrap() {
            lexeme += &self.next().unwrap().to_string();
        }
        let end = self.get_pos();
        Token {
            end,
            lexeme,
            kind: TokenKind::WhiteSpace,
            start,
        }
    }

    fn single_char(&mut self, kind: TokenKind) -> Token {
        let start = self.get_pos();
        let lexeme = String::from(self.next().unwrap());
        let end = self.get_pos();
        Token {
            kind,
            end,
            lexeme,
            start,
        }
    }

    fn symbol_or_keyword(&mut self) -> Option<Token> {
        let start = self.get_pos();
        let mut lexeme = String::new();

        while ('A'..='z').contains(&self.peek()?) {
            lexeme += &self.next()?.to_string();
        }
        let end = self.get_pos();

        let kind = match lexeme.as_str() {
            "let" => TokenKind::LetKeyword,
            "fn" => TokenKind::FnKeyword,
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

    #[test]
    fn lexing() {
        let code = String::from(
            "fn test() {
3
}",
        );
        for token in Lexer::new(code) {
            println!("{:?}", token);
        }
    }
}
