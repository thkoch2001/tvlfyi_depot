#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedChar(char),
}

#[derive(Debug)]
pub struct Error {
    pub line: usize,
    pub kind: ErrorKind,
}

pub fn report(loc: &str, err: &Error) {
    eprintln!("[line {}] Error {}: {:?}", err.line, loc, err.kind);
}
