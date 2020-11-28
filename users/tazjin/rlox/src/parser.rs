// This implements the grammar of Lox as described starting in the
// Crafting Interpreters chapter "Representing Code". Note that the
// upstream Java implementation works about Java being bad at value
// classes by writing a code generator for Java.
//
// My Rust implementation skips this step because it's unnecessary, we
// have real types.
use crate::scanner::{Token, TokenKind};

// AST

#[derive(Debug)]
struct Binary<'a> {
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
    operator: Token<'a>,
}

#[derive(Debug)]
struct Grouping<'a>(Box<Expr<'a>>);

#[derive(Debug)]
struct Literal(TokenKind);

#[derive(Debug)]
enum Expr<'a> {
    Binary(Binary<'a>),
    Grouping(Grouping<'a>),
    Literal(Literal),
}

// Parser

/*
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*/

struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    // recursive-descent parser functions

    fn expression(&mut self) -> Expr<'a> {
        self.equality()
    }

    fn equality(&mut self) -> Expr<'a> {
        let expr = self.comparison();
        unimplemented!()
    }

    fn comparison(&mut self) -> Expr<'a> {
        unimplemented!()
    }

    // internal helpers
    fn match_token(&mut self, oneof: &[TokenKind]) -> bool {
        for token in oneof {
            if self.check_token(token) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        return self.previous();
    }

    fn is_at_end(&self) -> bool {
        self.check_token(&TokenKind::Eof)
    }

    fn check_token(&self, token: &TokenKind) -> bool {
        self.peek().kind == *token
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
