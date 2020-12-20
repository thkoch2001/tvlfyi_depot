// This implements the grammar of Lox as described starting in the
// Crafting Interpreters chapter "Representing Code". Note that the
// upstream Java implementation works around Java being bad at value
// classes by writing a code generator for Java.
//
// My Rust implementation skips this step because it's unnecessary, we
// have real types.
use crate::errors::{Error, ErrorKind};
use crate::scanner::{Token, TokenKind};

// AST

#[derive(Debug)]
pub struct Binary<'a> {
    pub left: Box<Expr<'a>>,
    pub operator: Token<'a>,
    pub right: Box<Expr<'a>>,
}

#[derive(Debug)]
pub struct Grouping<'a>(pub Box<Expr<'a>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Number(f64),
    String(String),
    Nil,
}

#[derive(Debug)]
pub struct Unary<'a> {
    pub operator: Token<'a>,
    pub right: Box<Expr<'a>>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Binary(Binary<'a>),
    Grouping(Grouping<'a>),
    Literal(Literal),
    Unary(Unary<'a>),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Expr(Expr<'a>),
    Print(Expr<'a>),
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Stmt(Statement<'a>),
}

pub type Program<'a> = Vec<Declaration<'a>>;

// Parser

/*
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

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

type ExprResult<'a> = Result<Expr<'a>, Error>;
type StmtResult<'a> = Result<Statement<'a>, Error>;
type DeclResult<'a> = Result<Declaration<'a>, Error>;

impl<'a> Parser<'a> {
    // recursive-descent parser functions

    fn declaration(&mut self) -> DeclResult<'a> {
        Ok(Declaration::Stmt(self.statement()?))
    }

    fn statement(&mut self) -> StmtResult<'a> {
        if self.match_token(&[TokenKind::Print]) {
            self.print_statement()
        } else {
            self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> StmtResult<'a> {
        let expr = self.expression()?;
        self.consume(&TokenKind::Semicolon, ErrorKind::ExpectedSemicolon)?;
        Ok(Statement::Print(expr))
    }

    fn expr_statement(&mut self) -> StmtResult<'a> {
        let expr = self.expression()?;
        self.consume(&TokenKind::Semicolon, ErrorKind::ExpectedSemicolon)?;
        Ok(Statement::Expr(expr))
    }

    fn expression(&mut self) -> ExprResult<'a> {
        self.equality()
    }

    fn equality(&mut self) -> ExprResult<'a> {
        self.binary_operator(
            &[TokenKind::BangEqual, TokenKind::EqualEqual],
            Self::comparison,
        )
    }

    fn comparison(&mut self) -> ExprResult<'a> {
        self.binary_operator(
            &[
                TokenKind::Greater,
                TokenKind::GreaterEqual,
                TokenKind::Less,
                TokenKind::LessEqual,
            ],
            Self::term,
        )
    }

    fn term(&mut self) -> ExprResult<'a> {
        self.binary_operator(&[TokenKind::Minus, TokenKind::Plus], Self::factor)
    }

    fn factor(&mut self) -> ExprResult<'a> {
        self.binary_operator(&[TokenKind::Slash, TokenKind::Star], Self::unary)
    }

    fn unary(&mut self) -> ExprResult<'a> {
        if self.match_token(&[TokenKind::Bang, TokenKind::Minus]) {
            return Ok(Expr::Unary(Unary {
                operator: self.previous().clone(),
                right: Box::new(self.unary()?),
            }));
        }

        return self.primary();
    }

    fn primary(&mut self) -> ExprResult<'a> {
        let next = self.advance();
        let literal = match next.kind {
            TokenKind::True => Literal::Boolean(true),
            TokenKind::False => Literal::Boolean(false),
            TokenKind::Nil => Literal::Nil,
            TokenKind::Number(num) => Literal::Number(num),
            TokenKind::String(string) => Literal::String(string),

            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(&TokenKind::RightParen, ErrorKind::UnmatchedParens)?;
                return Ok(Expr::Grouping(Grouping(Box::new(expr))));
            }

            unexpected => {
                eprintln!("encountered {:?}", unexpected);
                return Err(Error {
                    line: next.line,
                    kind: ErrorKind::ExpectedExpression(next.lexeme.into_iter().collect()),
                });
            }
        };

        Ok(Expr::Literal(literal))
    }

    // internal helpers

    /// Check if the next token is in `oneof`, and advance if it is.
    fn match_token(&mut self, oneof: &[TokenKind]) -> bool {
        for token in oneof {
            if self.check_token(token) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    /// Return the next token and advance parser state.
    fn advance(&mut self) -> Token<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }

        return self.previous().clone();
    }

    fn is_at_end(&self) -> bool {
        self.check_token(&TokenKind::Eof)
    }

    /// Is the next token `token`?
    fn check_token(&self, token: &TokenKind) -> bool {
        self.peek().kind == *token
    }

    fn peek(&self) -> &Token<'a> {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token<'a> {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, kind: &TokenKind, err: ErrorKind) -> Result<(), Error> {
        if self.check_token(kind) {
            self.advance();
            return Ok(());
        }

        Err(Error {
            line: self.peek().line,
            kind: err,
        })
    }

    fn synchronise(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            match self.peek().kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,

                _ => {
                    self.advance();
                }
            }
        }
    }

    fn binary_operator(
        &mut self,
        oneof: &[TokenKind],
        each: fn(&mut Parser<'a>) -> ExprResult<'a>,
    ) -> ExprResult<'a> {
        let mut expr = each(self)?;

        while self.match_token(oneof) {
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: self.previous().clone(),
                right: Box::new(each(self)?),
            })
        }

        return Ok(expr);
    }
}

pub fn parse<'a>(tokens: Vec<Token<'a>>) -> Result<Program<'a>, Vec<Error>> {
    let mut parser = Parser { tokens, current: 0 };
    let mut program: Program<'a> = vec![];
    let mut errors: Vec<Error> = vec![];

    while !parser.is_at_end() {
        match parser.declaration() {
            Err(err) => {
                errors.push(err);
                parser.synchronise();
            }
            Ok(decl) => {
                program.push(decl);
            }
        }
    }

    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}
