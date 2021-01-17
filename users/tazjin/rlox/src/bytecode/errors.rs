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

pub type LoxResult<T> = Result<T, Error>;
