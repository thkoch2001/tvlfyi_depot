use super::chunk::Chunk;
use super::errors::{Error, ErrorKind, LoxResult};
use super::interner::{InternedStr, Interner};
use super::opcode::{CodeIdx, CodeOffset, ConstantIdx, OpCode, StackIdx};
use super::value::Value;
use crate::scanner::{self, Token, TokenKind};

#[cfg(feature = "disassemble")]
use super::chunk;

#[derive(Debug)]
enum Depth {
    Unitialised,
    At(usize),
}

impl Depth {
    fn above(&self, theirs: usize) -> bool {
        match self {
            Depth::Unitialised => false,
            Depth::At(ours) => *ours > theirs,
        }
    }

    fn below(&self, theirs: usize) -> bool {
        match self {
            Depth::Unitialised => false,
            Depth::At(ours) => *ours < theirs,
        }
    }
}

#[derive(Debug)]
struct Local {
    name: Token,
    depth: Depth,
}

#[derive(Debug, Default)]
struct Locals {
    locals: Vec<Local>,
    scope_depth: usize,
}

struct Compiler<T: Iterator<Item = Token>> {
    tokens: T,
    chunk: Chunk,
    panic: bool,
    errors: Vec<Error>,
    strings: Interner,
    locals: Locals,

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

        TokenKind::Identifier(_) => {
            ParseRule::new(Some(Compiler::variable), None, Precedence::None)
        }

        TokenKind::String(_) => {
            ParseRule::new(Some(Compiler::string), None, Precedence::None)
        }

        _ => ParseRule::new(None, None, Precedence::None),
    }
}

macro_rules! consume {
    ( $self:ident, $expected:pat, $err:expr ) => {
        match $self.current().kind {
            $expected => $self.advance(),
            _ => $self.error_at($self.current().line, $err),
        }
    };
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
        let idx = self.parse_variable()?;

        if self.match_token(&TokenKind::Equal) {
            self.expression()?;
        } else {
            self.emit_op(OpCode::OpNil);
        }

        self.expect_semicolon("expect ';' after variable declaration")?;
        self.define_variable(idx)
    }

    fn define_variable(&mut self, var: Option<ConstantIdx>) -> LoxResult<()> {
        if self.locals.scope_depth == 0 {
            self.emit_op(OpCode::OpDefineGlobal(
                var.expect("should be global"),
            ));
        } else {
            self.locals
                .locals
                .last_mut()
                .expect("fatal: variable not yet added at definition")
                .depth = Depth::At(self.locals.scope_depth);
        }

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
        } else if self.match_token(&TokenKind::If) {
            self.if_statement()
        } else if self.match_token(&TokenKind::LeftBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
            Ok(())
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

    fn begin_scope(&mut self) {
        self.locals.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        debug_assert!(self.locals.scope_depth > 0, "tried to end global scope");
        self.locals.scope_depth -= 1;

        while self.locals.locals.len() > 0
            && self.locals.locals[self.locals.locals.len() - 1]
                .depth
                .above(self.locals.scope_depth)
        {
            self.emit_op(OpCode::OpPop);
            self.locals.locals.remove(self.locals.locals.len() - 1);
        }
    }

    fn block(&mut self) -> LoxResult<()> {
        while !self.check(&TokenKind::RightBrace)
            && !self.check(&TokenKind::Eof)
        {
            self.declaration()?;
        }

        consume!(
            self,
            TokenKind::RightBrace,
            ErrorKind::ExpectedToken("Expected '}' after block.")
        );
        Ok(())
    }

    fn expression_statement(&mut self) -> LoxResult<()> {
        self.expression()?;
        self.expect_semicolon("expect ';' after expression")?;
        // TODO(tazjin): Why did I add this originally?
        // self.emit_op(OpCode::OpPop);
        Ok(())
    }

    fn if_statement(&mut self) -> LoxResult<()> {
        consume!(
            self,
            TokenKind::LeftParen,
            ErrorKind::ExpectedToken("Expected '(' after 'if'")
        );

        self.expression()?;

        consume!(
            self,
            TokenKind::RightParen,
            ErrorKind::ExpectedToken("Expected ')' after condition")
        );

        let then_jump = self.emit_op(OpCode::OpJumpPlaceholder(false));
        self.statement()?;
        self.patch_jump(then_jump);

        Ok(())
    }

    fn number(&mut self) -> LoxResult<()> {
        if let TokenKind::Number(num) = self.previous().kind {
            self.emit_constant(Value::Number(num), true);
            return Ok(());
        }

        unreachable!("internal parser error: entered number() incorrectly")
    }

    fn grouping(&mut self) -> LoxResult<()> {
        self.expression()?;
        consume!(
            self,
            TokenKind::RightParen,
            ErrorKind::ExpectedToken("Expected ')' after expression")
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
        };

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
                self.emit_op(OpCode::OpNot)
            }

            TokenKind::EqualEqual => self.emit_op(OpCode::OpEqual),
            TokenKind::Greater => self.emit_op(OpCode::OpGreater),

            TokenKind::GreaterEqual => {
                self.emit_op(OpCode::OpLess);
                self.emit_op(OpCode::OpNot)
            }

            TokenKind::Less => self.emit_op(OpCode::OpLess),
            TokenKind::LessEqual => {
                self.emit_op(OpCode::OpGreater);
                self.emit_op(OpCode::OpNot)
            }

            _ => unreachable!("only called for binary operator tokens"),
        };

        Ok(())
    }

    fn literal(&mut self) -> LoxResult<()> {
        match self.previous().kind {
            TokenKind::Nil => self.emit_op(OpCode::OpNil),
            TokenKind::True => self.emit_op(OpCode::OpTrue),
            TokenKind::False => self.emit_op(OpCode::OpFalse),
            _ => unreachable!("only called for literal value tokens"),
        };

        Ok(())
    }

    fn string(&mut self) -> LoxResult<()> {
        let val = match &self.previous().kind {
            TokenKind::String(s) => s.clone(),
            _ => unreachable!("only called for strings"),
        };

        let id = self.strings.intern(val);
        self.emit_constant(Value::String(id.into()), true);

        Ok(())
    }

    fn named_variable(&mut self, name: Token) -> LoxResult<()> {
        let local_idx = self.resolve_local(&name);

        let ident = if local_idx.is_some() {
            None
        } else {
            Some(self.identifier_constant(&name)?)
        };

        if self.match_token(&TokenKind::Equal) {
            self.expression()?;
            match local_idx {
                Some(idx) => self.emit_op(OpCode::OpSetLocal(idx)),
                None => self.emit_op(OpCode::OpSetGlobal(ident.unwrap())),
            };
        } else {
            match local_idx {
                Some(idx) => self.emit_op(OpCode::OpGetLocal(idx)),
                None => self.emit_op(OpCode::OpGetGlobal(ident.unwrap())),
            };
        }

        Ok(())
    }

    fn variable(&mut self) -> LoxResult<()> {
        let name = self.previous().clone();
        self.named_variable(name)
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

    fn identifier_str(&mut self, token: &Token) -> LoxResult<InternedStr> {
        let ident = match &token.kind {
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

    fn identifier_constant(&mut self, name: &Token) -> LoxResult<ConstantIdx> {
        let ident = self.identifier_str(name)?;
        Ok(self.emit_constant(Value::String(ident.into()), false))
    }

    fn resolve_local(&self, name: &Token) -> Option<StackIdx> {
        for (idx, local) in self.locals.locals.iter().enumerate().rev() {
            if name.lexeme == local.name.lexeme {
                if let Depth::Unitialised = local.depth {
                    // TODO(tazjin): *return* err
                    panic!("can't read variable in its own initialiser");
                }
                return Some(StackIdx(idx));
            }
        }

        None
    }

    fn add_local(&mut self, name: Token) {
        let local = Local {
            name,
            depth: Depth::Unitialised,
        };

        self.locals.locals.push(local);
    }

    fn declare_variable(&mut self) -> LoxResult<()> {
        if self.locals.scope_depth == 0 {
            return Ok(());
        }

        let name = self.previous().clone();

        for local in self.locals.locals.iter().rev() {
            if local.depth.below(self.locals.scope_depth) {
                break;
            }

            if name.lexeme == local.name.lexeme {
                return Err(Error {
                    kind: ErrorKind::VariableShadowed(name.lexeme.into()),
                    line: name.line,
                });
            }
        }

        self.add_local(name);
        Ok(())
    }

    fn parse_variable(&mut self) -> LoxResult<Option<ConstantIdx>> {
        consume!(
            self,
            TokenKind::Identifier(_),
            ErrorKind::ExpectedToken("expected identifier")
        );

        self.declare_variable()?;
        if self.locals.scope_depth > 0 {
            return Ok(None);
        }

        let name = self.previous().clone();
        let id = self.identifier_str(&name)?;
        Ok(Some(self.emit_constant(Value::String(id.into()), false)))
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

    fn emit_op(&mut self, op: OpCode) -> CodeIdx {
        let line = self.previous().line;
        self.current_chunk().add_op(op, line)
    }

    fn emit_constant(&mut self, val: Value, with_op: bool) -> ConstantIdx {
        let idx = ConstantIdx(self.chunk.add_constant(val));

        if with_op {
            self.emit_op(OpCode::OpConstant(idx));
        }

        idx
    }

    fn patch_jump(&mut self, idx: CodeIdx) {
        let offset = CodeOffset(self.chunk.code.len() - idx.0 - 1);

        if let OpCode::OpJumpPlaceholder(false) = self.chunk.code[idx.0] {
            self.chunk.code[idx.0] = OpCode::OpJumpIfFalse(offset);
            return;
        }

        panic!(
            "attempted to patch unsupported op: {:?}",
            self.chunk.code[idx.0]
        );
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
        consume!(self, TokenKind::Semicolon, ErrorKind::ExpectedToken(msg));
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
        locals: Default::default(),
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
