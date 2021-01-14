use crate::interpreter::Value;

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

    // This variant is not an error, rather it is used for
    // short-circuiting out of a function body that hits a `return`
    // statement.
    //
    // It's implemented this way because in the original book the
    // author uses exceptions for control flow, and this is the
    // closest equivalent that I had available without diverging too
    // much.
    FunctionReturn(Value),
}

#[derive(Debug)]
pub struct Error {
    pub line: usize,
    pub kind: ErrorKind,
}

pub fn report(err: &Error) {
    eprintln!("[line {}] Error: {:?}", err.line, err.kind);
}
