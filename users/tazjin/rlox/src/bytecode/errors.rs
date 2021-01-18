use std::fmt;

#[derive(Debug)]
pub enum ErrorKind {
    // CompileError,
    // RuntimeError,
    InternalError(&'static str),
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line NYI] Error: {:?}", self.kind)
    }
}

pub type LoxResult<T> = Result<T, Error>;
