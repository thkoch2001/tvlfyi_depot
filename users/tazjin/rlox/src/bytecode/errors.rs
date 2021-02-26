use crate::scanner::ScannerError;

use std::fmt;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
    InternalError(&'static str),
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

pub type LoxResult<T> = Result<T, Error>;
