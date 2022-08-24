use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    DuplicateAttrsKey {
        key: String,
    },

    AttributeNotFound {
        name: String,
    },

    TypeError {
        expected: &'static str,
        actual: &'static str,
    },

    Incomparable {
        lhs: &'static str,
        rhs: &'static str,
    },

    // Resolving a user-supplied path literal failed in some way.
    PathResolution(String),

    // Dynamic keys are not allowed in let.
    DynamicKeyInLet(rnix::SyntaxNode),

    // Unknown variable in statically known scope.
    UnknownStaticVariable,

    // Unknown variable in dynamic scope (with, rec, ...).
    UnknownDynamicVariable(String),

    // Attempt to call something that is not callable.
    NotCallable,

    ParseErrors(Vec<rnix::parser::ParseError>),

    AssertionFailed,

    // These are user-generated errors through builtins.
    Throw(String),
    Abort(String),
}

#[derive(Clone, Debug)]
pub struct Error {
    pub node: Option<rnix::SyntaxNode>,
    pub kind: ErrorKind,
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { node: None, kind }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.kind)
    }
}

pub type EvalResult<T> = Result<T, Error>;
