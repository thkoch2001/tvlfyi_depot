use super::chunk::Chunk;
use super::errors::{Error, ErrorKind, LoxResult};
use super::opcode::OpCode;
use crate::scanner;

struct Compiler<T: Iterator<Item = scanner::Token>> {
    // panic: bool,
    errors: Vec<Error>,
    tokens: T,
    chunk: Chunk,

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
        let line = self.previous().line;
        self.current_chunk().add_op(OpCode::OpReturn, line);
        Ok(())
    }

    fn previous(&self) -> &scanner::Token {
        self.previous
            .as_ref()
            .expect("invalid internal compiler state: missing previous token")
    }
}

pub fn compile(code: &str) -> Result<Chunk, Vec<Error>> {
    let chars = code.chars().collect::<Vec<char>>();
    let tokens = scanner::scan(&chars).map_err(|errors| {
        errors.into_iter().map(Into::into).collect::<Vec<Error>>()
    })?;

    let mut compiler = Compiler {
        tokens: tokens.into_iter().peekable(),
        errors: vec![],
        current: None,
        previous: None,
        chunk: Default::default(),
    };

    compiler.compile()?;

    if compiler.errors.is_empty() {
        Ok(unimplemented!())
    } else {
        Err(compiler.errors)
    }
}
