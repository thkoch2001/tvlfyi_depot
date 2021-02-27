use super::chunk::Chunk;
use super::errors::{Error, ErrorKind, LoxResult};
use super::opcode::OpCode;
use crate::scanner;

struct Compiler<T: Iterator<Item = scanner::Token>> {
    tokens: T,
    chunk: Chunk,
    panic: bool,
    errors: Vec<Error>,

    // TODO(tazjin): Restructure so that these don't need to be Option?
    current: Option<scanner::Token>,
    previous: Option<scanner::Token>,
}

impl<T: Iterator<Item = scanner::Token>> Compiler<T> {
    fn compile(&mut self) -> LoxResult<()> {
        self.advance();
        self.expression();
        self.consume(
            &scanner::TokenKind::Eof,
            ErrorKind::ExpectedToken("Expected end of expression"),
        )?;

        self.end_compiler()
    }

    fn advance(&mut self) {
        self.previous = self.current.take();
        self.current = self.tokens.next();
    }

    fn expression(&mut self) {
        unimplemented!()
    }

    fn consume(
        &mut self,
        expected: &scanner::TokenKind,
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

    fn previous(&self) -> &scanner::Token {
        self.previous
            .as_ref()
            .expect("invalid internal compiler state: missing previous token")
    }

    fn error_at(&mut self, token: &scanner::Token, kind: ErrorKind) {
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
