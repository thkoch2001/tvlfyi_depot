use crate::errors::{Error, ErrorKind};

#[derive(Debug)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Special things
    Eof,
}

#[derive(Debug)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a [char],
    // literal: Object, // TODO(tazjin): Uhh?
    line: usize,
}

struct Scanner<'a> {
    source: &'a [char],
    tokens: Vec<Token<'a>>,
    errors: Vec<Error>,
    start: usize,   // offset of first character in current lexeme
    current: usize, // current offset into source
    line: usize,    // current line in source
}

impl<'a> Scanner<'a> {
    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source[self.current - 1]
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme = &self.source[self.start..self.current];
        self.tokens.push(Token {
            kind,
            lexeme,
            line: self.line,
        })
    }

    fn scan_token(&mut self) {
        match self.advance() {
            // simple single-character tokens
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            '*' => self.add_token(TokenKind::Star),

            // possible multi-character tokens
            '!' => self.add_if_next('=', TokenKind::BangEqual, TokenKind::Bang),
            '=' => self.add_if_next('=', TokenKind::EqualEqual, TokenKind::Equal),
            '<' => self.add_if_next('=', TokenKind::LessEqual, TokenKind::Less),
            '>' => self.add_if_next('=', TokenKind::GreaterEqual, TokenKind::Greater),

            '/' => {
                // support comments until EOL by discarding characters
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }

            // ignore whitespace
            ' ' => {}
            '\r' => {}
            '\t' => {}
            '\n' => self.line += 1,

            '"' => self.scan_string(),

            digit if digit.is_digit(10) => self.scan_number(),

            unexpected => self.errors.push(Error {
                line: self.line,
                kind: ErrorKind::UnexpectedChar(unexpected),
            }),
        };
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn add_if_next(&mut self, expected: char, then: TokenKind, or: TokenKind) {
        if self.match_next(expected) {
            self.add_token(then);
        } else {
            self.add_token(or);
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        } else {
            return self.source[self.current];
        }
    }

    fn peek_next(&self) -> char {
        if (self.current + 1 >= self.source.len()) {
            return '\0';
        } else {
            return self.source[self.current + 1];
        }
    }

    fn scan_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            self.errors.push(Error {
                line: self.line,
                kind: ErrorKind::UnterminatedString,
            });
            return;
        }

        // closing '"'
        self.advance();

        // add token without surrounding quotes
        let string: String = self.source[(self.start + 1)..(self.current - 1)]
            .iter()
            .collect();
        self.add_token(TokenKind::String(string));
    }

    fn scan_number(&mut self) {
        while self.peek().is_digit(10) {
            self.advance();
        }

        // Look for a fractional part
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            // consume '.'
            self.advance();

            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        let num: f64 = self.source[self.start..self.current]
            .iter()
            .collect::<String>()
            .parse()
            .expect("float parsing should always work");

        self.add_token(TokenKind::Number(num));
    }

    fn scan_tokens(mut self) -> Vec<Token<'a>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        return self.tokens;
    }
}

pub fn scan<'a>(input: &'a [char]) -> Vec<Token<'a>> {
    let scanner = Scanner {
        source: &input,
        tokens: vec![],
        errors: vec![],
        start: 0,
        current: 0,
        line: 0,
    };

    return scanner.scan_tokens();
}
