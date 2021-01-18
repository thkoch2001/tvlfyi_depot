use crate::treewalk::interpreter::Value;
use std::fmt;

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
    StaticError(String),

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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {:?}", self.line, self.kind)
    }
}
