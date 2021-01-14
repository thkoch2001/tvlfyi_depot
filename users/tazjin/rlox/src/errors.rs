#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
    UnmatchedParens,
    ExpectedExpression(String),
    ExpectedSemicolon,
    ExpectedClosingBrace,
    ExpectedToken(&'static str),
    TypeError(String),
    UndefinedVariable(String),
    InternalError(String),
    InvalidAssignmentTarget(String),
    RuntimeError(String),
}

#[derive(Debug)]
pub struct Error {
    pub line: usize,
    pub kind: ErrorKind,
}

pub fn report(err: &Error) {
    eprintln!("[line {}] Error: {:?}", err.line, err.kind);
}
