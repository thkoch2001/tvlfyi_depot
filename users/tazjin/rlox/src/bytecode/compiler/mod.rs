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

        _ => ParseRule::new(None, None, Precedence::None),
    }
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
    fn number(&mut self) -> LoxResult<()> {
        let num = unimplemented!("get out of previous()");
        self.emit_constant(num);
    }

    fn grouping(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.consume(
            &TokenKind::RightParen,
            ErrorKind::ExpectedToken("Expected ')' after expression"),
        )
    }

    fn unary(&mut self) -> LoxResult<()> {
        // TODO(tazjin): Avoid clone
        let kind = self.previous().kind.clone();

        // Compile the operand
        self.parse_precedence(Precedence::Unary)?;

        // Emit operator instruction
        match kind {
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
        self.parse_precedence(rule.precedence)?;

        // Emit operator instruction
        match operator {
            TokenKind::Minus => self.emit_op(OpCode::OpSubtract),
            TokenKind::Plus => self.emit_op(OpCode::OpAdd),
            TokenKind::Star => self.emit_op(OpCode::OpMultiply),
            TokenKind::Slash => self.emit_op(OpCode::OpDivide),
            _ => unreachable!("only called for binary operator tokens"),
        }

        unimplemented!()
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> LoxResult<()> {
        self.advance();
        let rule: ParseRule<T> = rule_for(&self.previous().kind);

        if let Some(func) = rule.prefix {
            func(self)?;
        }

        unimplemented!("expect expression?")
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
