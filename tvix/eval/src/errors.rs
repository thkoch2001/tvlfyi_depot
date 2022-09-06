use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum ErrorKind {
    DuplicateAttrsKey {
        key: String,
    },

    /// Attempted to specify an invalid key type (e.g. integer) in a
    /// dynamic attribute name.
    InvalidAttributeName {
        given: &'static str,
    },

    AttributeNotFound {
        name: String,
    },

    // Attempted to index into a list beyond its boundaries.
    IndexOutOfBounds {
        index: usize,
    },

    TypeError {
        expected: &'static str,
        actual: &'static str,
    },

    Incomparable {
        lhs: &'static str,
        rhs: &'static str,
    },

    /// Resolving a user-supplied path literal failed in some way.
    PathResolution(String),

    /// Dynamic keys are not allowed in let.
    DynamicKeyInLet(rnix::SyntaxNode),

    /// Unknown variable in statically known scope.
    UnknownStaticVariable,

    /// Unknown variable in dynamic scope (with, rec, ...).
    UnknownDynamicVariable(String),

    /// User is defining the same variable twice at the same depth.
    VariableAlreadyDefined(String),

    /// Attempt to call something that is not callable.
    NotCallable,

    /// Infinite recursion encountered while forcing thunks.
    InfiniteRecursion,

    ParseErrors(Vec<rnix::parser::ParseError>),

    AssertionFailed,

    /// These are user-generated errors through builtins.
    Throw(String),
    Abort(String),

    /// An error occured while forcing a thunk, and needs to be
    /// chained up.
    ThunkForce(Box<Error>),

    /// Tvix internal warning for features triggered by users that are
    /// not actually implemented yet, and without which eval can not
    /// proceed.
    NotImplemented(&'static str),
}

impl From<std::io::Error> for ErrorKind {
    fn from(e: std::io::Error) -> Self {
        ErrorKind::Abort(e.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: codemap::Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.kind)
    }
}

pub type EvalResult<T> = Result<T, Error>;
