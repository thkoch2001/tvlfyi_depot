use crate::scanner::ScannerError;

use std::fmt;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
    ExpectedToken(&'static str),
    InternalError(&'static str),
    TypeError(String),
    VariableShadowed(String),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub line: usize,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line NYI] Error: {:?}", self.kind)
    }
}

impl From<ScannerError> for Error {
    fn from(err: ScannerError) -> Self {
        match err {
            ScannerError::UnexpectedChar { line, unexpected } => Error {
                line,
                kind: ErrorKind::UnexpectedChar(unexpected),
            },

            ScannerError::UnterminatedString { line } => Error {
                line,
                kind: ErrorKind::UnterminatedString,
            },
        }
    }
}

// Convenience implementation as we're often dealing with vectors of
// errors (to report as many issues as possible before terminating)
impl From<Error> for Vec<Error> {
    fn from(err: Error) -> Self {
        vec![err]
    }
}

pub type LoxResult<T> = Result<T, Error>;
