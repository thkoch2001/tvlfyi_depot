use super::chunk::Chunk;
use super::errors::{Error, ErrorKind, LoxResult};
use super::interner::{InternedStr, Interner};
use super::opcode::OpCode;
use super::value::Value;
use crate::scanner::{self, Token, TokenKind};

#[cfg(feature = "disassemble")]
use super::chunk;

struct Compiler<T: Iterator<Item = Token>> {
    tokens: T,
    chunk: Chunk,
    panic: bool,
    errors: Vec<Error>,
    strings: Interner,

    current: Option<Token>,
    previous: Option<Token>,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

type ParseFn<T> = fn(&mut Compiler<T>) -> LoxResult<()>;

struct ParseRule<T: Iterator<Item = Token>> {
    prefix: Option<ParseFn<T>>,
    infix: Option<ParseFn<T>>,
    precedence: Precedence,
}

impl<T: Iterator<Item = Token>> ParseRule<T> {
    fn new(
        prefix: Option<ParseFn<T>>,
        infix: Option<ParseFn<T>>,
        precedence: Precedence,
    ) -> Self {
        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }
}

impl Precedence {
    // Return the next highest precedence, if there is one.
    fn next(&self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => panic!(
                "invalid parser state: no higher precedence than Primary"
            ),
        }
    }
}

fn rule_for<T: Iterator<Item = Token>>(token: &TokenKind) -> ParseRule<T> {
    match token {
        TokenKind::LeftParen => {
            ParseRule::new(Some(Compiler::grouping), None, Precedence::None)
        }

        TokenKind::Minus => ParseRule::new(
            Some(Compiler::unary),
            Some(Compiler::binary),
            Precedence::Term,
        ),

        TokenKind::Plus => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Term)
        }

        TokenKind::Slash => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Factor)
        }

        TokenKind::Star => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Factor)
        }

        TokenKind::Number(_) => {
            ParseRule::new(Some(Compiler::number), None, Precedence::None)
        }

        TokenKind::True => {
            ParseRule::new(Some(Compiler::literal), None, Precedence::None)
        }

        TokenKind::False => {
            ParseRule::new(Some(Compiler::literal), None, Precedence::None)
        }

        TokenKind::Nil => {
            ParseRule::new(Some(Compiler::literal), None, Precedence::None)
        }

        TokenKind::Bang => {
            ParseRule::new(Some(Compiler::unary), None, Precedence::None)
        }

        TokenKind::BangEqual => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Equality)
        }

        TokenKind::EqualEqual => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Equality)
        }

        TokenKind::Greater => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)
        }

        TokenKind::GreaterEqual => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)
        }

        TokenKind::Less => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)
        }

        TokenKind::LessEqual => {
            ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison)
        }

        TokenKind::String(_) => {
            ParseRule::new(Some(Compiler::string), None, Precedence::None)
        }

        _ => ParseRule::new(None, None, Precedence::None),
    }
}

impl<T: Iterator<Item = Token>> Compiler<T> {
    fn compile(&mut self) -> LoxResult<()> {
        self.advance();

        while !self.match_token(&TokenKind::Eof) {
            self.declaration()?;
        }

        self.end_compiler()
    }

    fn advance(&mut self) {
        self.previous = self.current.take();
        self.current = self.tokens.next();
    }

    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn var_declaration(&mut self) -> LoxResult<()> {
        let global = self.parse_variable()?;

        if self.match_token(&TokenKind::Equal) {
            self.expression()?;
        } else {
            self.emit_op(OpCode::OpNil);
        }

        self.expect_semicolon("expect ';' after variable declaration")?;
        self.define_variable(global)
    }

    fn define_variable(&mut self, var: usize) -> LoxResult<()> {
        self.emit_op(OpCode::OpDefineGlobal(var));
        Ok(())
    }

    fn declaration(&mut self) -> LoxResult<()> {
        if self.match_token(&TokenKind::Var) {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }

        if self.panic {
            self.synchronise();
        }

        Ok(())
    }

    fn statement(&mut self) -> LoxResult<()> {
        if self.match_token(&TokenKind::Print) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.expect_semicolon("expect ';' after print statement")?;
        self.emit_op(OpCode::OpPrint);
        Ok(())
    }

    fn expression_statement(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.expect_semicolon("expect ';' after expression")?;
        self.emit_op(OpCode::OpPop);
        Ok(())
    }

    fn number(&mut self) -> LoxResult<()> {
        if let TokenKind::Number(num) = self.previous().kind {
            self.emit_constant(Value::Number(num));
            return Ok(());
        }

        unreachable!("internal parser error: entered number() incorrectly")
    }

    fn grouping(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.consume(
            &TokenKind::RightParen,
            ErrorKind::ExpectedToken("Expected ')' after expression"),
        );
        Ok(())
    }

    fn unary(&mut self) -> LoxResult<()> {
        // TODO(tazjin): Avoid clone
        let kind = self.previous().kind.clone();

        // Compile the operand
        self.parse_precedence(Precedence::Unary)?;

        // Emit operator instruction
        match kind {
            TokenKind::Bang => self.emit_op(OpCode::OpNot),
            TokenKind::Minus => self.emit_op(OpCode::OpNegate),
            _ => unreachable!("only called for unary operator tokens"),
        }

        Ok(())
    }

    fn binary(&mut self) -> LoxResult<()> {
        // Remember the operator
        let operator = self.previous().kind.clone();

        // Compile the right operand
        let rule: ParseRule<T> = rule_for(&operator);
        self.parse_precedence(rule.precedence.next())?;

        // Emit operator instruction
        match operator {
            TokenKind::Minus => self.emit_op(OpCode::OpSubtract),
            TokenKind::Plus => self.emit_op(OpCode::OpAdd),
            TokenKind::Star => self.emit_op(OpCode::OpMultiply),
            TokenKind::Slash => self.emit_op(OpCode::OpDivide),

            TokenKind::BangEqual => {
                self.emit_op(OpCode::OpEqual);
                self.emit_op(OpCode::OpNot);
            }

            TokenKind::EqualEqual => self.emit_op(OpCode::OpEqual),
            TokenKind::Greater => self.emit_op(OpCode::OpGreater),

            TokenKind::GreaterEqual => {
                self.emit_op(OpCode::OpLess);
                self.emit_op(OpCode::OpNot);
            }

            TokenKind::Less => self.emit_op(OpCode::OpLess),
            TokenKind::LessEqual => {
                self.emit_op(OpCode::OpGreater);
                self.emit_op(OpCode::OpNot);
            }

            _ => unreachable!("only called for binary operator tokens"),
        }

        Ok(())
    }

    fn literal(&mut self) -> LoxResult<()> {
        match self.previous().kind {
            TokenKind::Nil => self.emit_op(OpCode::OpNil),
            TokenKind::True => self.emit_op(OpCode::OpTrue),
            TokenKind::False => self.emit_op(OpCode::OpFalse),
            _ => unreachable!("only called for literal value tokens"),
        }

        Ok(())
    }

    fn string(&mut self) -> LoxResult<()> {
        let val = match &self.previous().kind {
            TokenKind::String(s) => s.clone(),
            _ => unreachable!("only called for strings"),
        };

        let id = self.strings.intern(val);
        self.emit_constant(Value::String(id.into()));

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> LoxResult<()> {
        self.advance();
        let rule: ParseRule<T> = rule_for(&self.previous().kind);
        let prefix_fn = match rule.prefix {
            None => unimplemented!("expected expression or something, unclear"),
            Some(func) => func,
        };

        prefix_fn(self)?;

        while precedence <= rule_for::<T>(&self.current().kind).precedence {
            self.advance();
            match rule_for::<T>(&self.previous().kind).infix {
                Some(func) => {
                    func(self)?;
                }
                None => {
                    unreachable!("invalid compiler state: error in parse rules")
                }
            }
        }

        Ok(())
    }

    fn identifier_str(
        &mut self,
        token_fn: fn(&Self) -> &Token,
    ) -> LoxResult<InternedStr> {
        let ident = match &token_fn(self).kind {
            TokenKind::Identifier(ident) => ident.to_string(),
            _ => {
                return Err(Error {
                    line: self.current().line,
                    kind: ErrorKind::ExpectedToken("Expected identifier"),
                })
            }
        };

        Ok(self.strings.intern(ident))
    }

    fn parse_variable(&mut self) -> LoxResult<usize> {
        let id = self.identifier_str(Self::current)?;
        Ok(self.emit_constant(Value::String(id.into())))
    }

    fn consume(&mut self, expected: &TokenKind, err: ErrorKind) {
        if self.current().kind == *expected {
            self.advance();
            return;
        }

        self.error_at(self.current().line, err);
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn end_compiler(&mut self) -> LoxResult<()> {
        self.emit_op(OpCode::OpReturn);

        #[cfg(feature = "disassemble")]
        {
            chunk::disassemble_chunk(&self.chunk);
            println!("== compilation finished ==");
        }

        Ok(())
    }

    fn emit_op(&mut self, op: OpCode) {
        let line = self.previous().line;
        self.current_chunk().add_op(op, line);
    }

    fn emit_constant(&mut self, val: Value) -> usize {
        let idx = self.chunk.add_constant(val);
        self.emit_op(OpCode::OpConstant(idx));
        idx
    }

    fn previous(&self) -> &Token {
        self.previous
            .as_ref()
            .expect("invalid internal compiler state: missing previous token")
    }

    fn current(&self) -> &Token {
        self.current
            .as_ref()
            .expect("invalid internal compiler state: missing current token")
    }

    fn error_at(&mut self, line: usize, kind: ErrorKind) {
        if self.panic {
            return;
        }

        self.panic = true;
        self.errors.push(Error { kind, line })
    }

    fn match_token(&mut self, token: &TokenKind) -> bool {
        if !self.check(token) {
            return false;
        }

        self.advance();
        true
    }

    fn check(&self, token: &TokenKind) -> bool {
        return self.current().kind == *token;
    }

    fn synchronise(&mut self) {
        self.panic = false;

        while self.current().kind != TokenKind::Eof {
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            match self.current().kind {
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

    fn expect_semicolon(&mut self, msg: &'static str) -> LoxResult<()> {
        self.consume(&TokenKind::Semicolon, ErrorKind::ExpectedToken(msg));
        Ok(())
    }
}

pub fn compile(code: &str) -> Result<(Interner, Chunk), Vec<Error>> {
    let chars = code.chars().collect::<Vec<char>>();
    let tokens = scanner::scan(&chars).map_err(|errors| {
        errors.into_iter().map(Into::into).collect::<Vec<Error>>()
    })?;

    let mut compiler = Compiler {
        tokens: tokens.into_iter().peekable(),
        chunk: Default::default(),
        panic: false,
        errors: vec![],
        strings: Interner::with_capacity(1024),
        current: None,
        previous: None,
    };

    compiler.compile()?;

    if compiler.errors.is_empty() {
        Ok((compiler.strings, compiler.chunk))
    } else {
        Err(compiler.errors)
    }
}
