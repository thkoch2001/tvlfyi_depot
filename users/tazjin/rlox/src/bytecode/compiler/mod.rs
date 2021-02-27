use super::chunk::Chunk;
use super::errors::{Error, ErrorKind, LoxResult};
use super::opcode::OpCode;
use super::value::Value;
use crate::scanner::{self, Token, TokenKind};

#[cfg(test)]
mod tests;

struct Compiler<T: Iterator<Item = Token>> {
    tokens: T,
    chunk: Chunk,
    panic: bool,
    errors: Vec<Error>,

    // TODO(tazjin): Restructure so that these don't need to be Option?
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

impl<T: Iterator<Item = Token>> Compiler<T> {
    fn compile(&mut self) -> LoxResult<()> {
        self.advance();
        self.expression()?;
        self.consume(
            &TokenKind::Eof,
            ErrorKind::ExpectedToken("Expected end of expression"),
        )?;

        self.end_compiler()
    }

    fn advance(&mut self) {
        self.previous = self.current.take();
        self.current = self.tokens.next();
    }

    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    // TODO(tazjin): Assumption is that we have access to the previous
    // token wherever this ends up invoked. True?
    fn number(&mut self, num: f64) {
        self.emit_constant(num);
    }

    fn grouping(&mut self, num: f64) -> LoxResult<()> {
        self.expression()?;
        self.consume(
            &TokenKind::RightParen,
            ErrorKind::ExpectedToken("Expected ')' after expression"),
        )
    }

    fn unary(&mut self, kind: &TokenKind) -> LoxResult<()> {
        // Compile the operand
        self.parse_precedence(Precedence::Unary)?;

        // Emit operator instruction
        match kind {
            TokenKind::Minus => self.emit_op(OpCode::OpNegate),
            _ => unreachable!("only called for unary operator tokens"),
        }

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> LoxResult<()> {
        unimplemented!("what goes here?")
    }

    fn consume(
        &mut self,
        expected: &TokenKind,
        err: ErrorKind,
    ) -> LoxResult<()> {
        unimplemented!()
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn end_compiler(&mut self) -> LoxResult<()> {
        self.emit_op(OpCode::OpReturn);
        Ok(())
    }

    fn emit_op(&mut self, op: OpCode) {
        let line = self.previous().line;
        self.current_chunk().add_op(op, line);
    }

    fn emit_constant(&mut self, val: Value) {
        let idx = self.chunk.add_constant(val);
        self.emit_op(OpCode::OpConstant(idx));
    }

    fn previous(&self) -> &Token {
        self.previous
            .as_ref()
            .expect("invalid internal compiler state: missing previous token")
    }

    fn error_at(&mut self, token: &Token, kind: ErrorKind) {
        if self.panic {
            return;
        }

        self.panic = true;
        self.errors.push(Error {
            kind,
            line: token.line,
        })
    }
}

pub fn compile(code: &str) -> Result<Chunk, Vec<Error>> {
    let chars = code.chars().collect::<Vec<char>>();
    let tokens = scanner::scan(&chars).map_err(|errors| {
        errors.into_iter().map(Into::into).collect::<Vec<Error>>()
    })?;

    let mut compiler = Compiler {
        tokens: tokens.into_iter().peekable(),
        chunk: Default::default(),
        panic: false,
        errors: vec![],
        current: None,
        previous: None,
    };

    compiler.compile()?;

    if compiler.errors.is_empty() {
        Ok(unimplemented!())
    } else {
        Err(compiler.errors)
    }
}
