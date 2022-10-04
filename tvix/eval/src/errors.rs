use crate::value::CoercionKind;
use std::path::PathBuf;
use std::rc::Rc;
use std::{fmt::Display, num::ParseIntError};

use codemap::Span;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use smol_str::SmolStr;

use crate::{SourceCode, Value};

#[derive(Clone, Debug)]
pub enum ErrorKind {
    /// These are user-generated errors through builtins.
    Throw(String),
    Abort(String),
    AssertionFailed,

    DuplicateAttrsKey {
        key: String,
    },

    /// Attempted to specify an invalid key type (e.g. integer) in a
    /// dynamic attribute name.
    InvalidAttributeName(Value),

    AttributeNotFound {
        name: String,
    },

    /// Attempted to index into a list beyond its boundaries.
    IndexOutOfBounds {
        index: i64,
    },

    /// Attempted to call `builtins.tail` on an empty list.
    TailEmptyList,

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

    /// Dynamic keys are not allowed in some scopes.
    DynamicKeyInScope(&'static str),

    /// Unknown variable in statically known scope.
    UnknownStaticVariable,

    /// Unknown variable in dynamic scope (with, rec, ...).
    UnknownDynamicVariable(String),

    /// User is defining the same variable twice at the same depth.
    VariableAlreadyDefined(Span),

    /// Attempt to call something that is not callable.
    NotCallable(&'static str),

    /// Infinite recursion encountered while forcing thunks.
    InfiniteRecursion,

    ParseErrors(Vec<rnix::parser::ParseError>),

    /// An error occured while forcing a thunk, and needs to be
    /// chained up.
    ThunkForce(Box<Error>),

    /// Given type can't be coerced to a string in the respective context
    NotCoercibleToString {
        from: &'static str,
        kind: CoercionKind,
    },

    /// The given string doesn't represent an absolute path
    NotAnAbsolutePath(PathBuf),

    /// An error occurred when parsing an integer
    ParseIntError(ParseIntError),

    /// A negative integer was used as a value representing length.
    NegativeLength {
        length: i64,
    },

    // Errors specific to nested attribute sets and merges thereof.
    /// Nested attributes can not be merged with an inherited value.
    UnmergeableInherit {
        name: SmolStr,
    },

    /// Nested attributes can not be merged with values that are not
    /// literal attribute sets.
    UnmergeableValue,

    /// Tvix failed to read a file from disk for some reason.
    ReadFileError {
        path: PathBuf,
        error: Rc<std::io::Error>,
    },

    /// Parse errors occured while importing a file.
    ImportParseError {
        path: PathBuf,
        errors: Vec<rnix::parser::ParseError>,
    },

    /// Compilation errors occured while importing a file.
    ImportCompilerError {
        path: PathBuf,
        errors: Vec<Error>,
    },

    /// Tvix internal warning for features triggered by users that are
    /// not actually implemented yet, and without which eval can not
    /// proceed.
    NotImplemented(&'static str),
}

impl From<ParseIntError> for ErrorKind {
    fn from(e: ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}

/// Implementation used if errors occur while forcing thunks (which
/// can potentially be threaded through a few contexts, i.e. nested
/// thunks).
impl From<Error> for ErrorKind {
    fn from(e: Error) -> Self {
        Self::ThunkForce(Box::new(e))
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.kind)
    }
}

pub type EvalResult<T> = Result<T, Error>;

impl Error {
    pub fn fancy_format_str(&self, source: &SourceCode) -> String {
        let mut out = vec![];
        Emitter::vec(&mut out, Some(&*source.codemap())).emit(&[self.diagnostic()]);
        String::from_utf8_lossy(&out).to_string()
    }

    /// Render a fancy, human-readable output of this error and print
    /// it to stderr.
    pub fn fancy_format_stderr(&self, source: &SourceCode) {
        Emitter::stderr(ColorConfig::Auto, Some(&*source.codemap())).emit(&[self.diagnostic()]);
    }

    /// Create the optional span label displayed as an annotation on
    /// the underlined span of the error.
    fn span_label(&self) -> Option<String> {
        None
    }

    /// Create the primary error message displayed to users.
    fn message(&self) -> String {
        match &self.kind {
            ErrorKind::Throw(msg) => format!("error thrown: {}", msg),
            ErrorKind::Abort(msg) => format!("evaluation aborted: {}", msg),
            ErrorKind::AssertionFailed => "assertion failed".to_string(),

            ErrorKind::DuplicateAttrsKey { key } => {
                format!("attribute key '{}' already defined", key)
            }

            ErrorKind::InvalidAttributeName(val) => format!(
                "found attribute name '{}' of type '{}', but attribute names must be strings",
                val,
                val.type_of()
            ),

            ErrorKind::AttributeNotFound { name } => format!(
                "attribute with name '{}' could not be found in the set",
                name
            ),

            ErrorKind::IndexOutOfBounds { index } => {
                format!("list index '{}' is out of bounds", index)
            }

            ErrorKind::TailEmptyList => "'tail' called on an empty list".to_string(),

            ErrorKind::TypeError { expected, actual } => format!(
                "expected value of type '{}', but found a '{}'",
                expected, actual
            ),

            ErrorKind::Incomparable { lhs, rhs } => {
                format!("can not compare a {} with a {}", lhs, rhs)
            }

            ErrorKind::PathResolution(err) => format!("could not resolve path: {}", err),

            ErrorKind::DynamicKeyInScope(scope) => {
                format!("dynamically evaluated keys are not allowed in {}", scope)
            }

            ErrorKind::UnknownStaticVariable => "variable not found".to_string(),

            ErrorKind::UnknownDynamicVariable(name) => format!(
                r#"variable '{}' could not be found

Note that this occured within a `with`-expression. The problem may be related
to a missing value in the attribute set(s) included via `with`."#,
                name
            ),

            ErrorKind::VariableAlreadyDefined(_) => "variable has already been defined".to_string(),

            ErrorKind::NotCallable(other_type) => {
                format!(
                    "only functions and builtins can be called, but this is a '{}'",
                    other_type
                )
            }

            ErrorKind::InfiniteRecursion => "infinite recursion encountered".to_string(),

            // TODO(tazjin): these errors should actually end up with
            // individual spans etc.
            ErrorKind::ParseErrors(errors) => format!("failed to parse Nix code: {:?}", errors),

            // TODO(tazjin): trace through the whole chain of thunk
            // forcing errors with secondary spans, instead of just
            // delegating to the inner error
            ErrorKind::ThunkForce(err) => err.message(),

            ErrorKind::NotCoercibleToString { kind, from } => {
                let kindly = match kind {
                    CoercionKind::Strong => "strongly",
                    CoercionKind::Weak => "weakly",
                };

                let hint = if *from == "set" {
                    ", missing a `__toString` or `outPath` attribute"
                } else {
                    ""
                };

                format!("cannot ({kindly}) coerce {from} to a string{hint}")
            }

            ErrorKind::NotAnAbsolutePath(given) => {
                format!(
                    "string {} doesn't represent an absolute path",
                    given.to_string_lossy()
                )
            }

            ErrorKind::ParseIntError(err) => {
                format!("invalid integer: {}", err)
            }

            ErrorKind::NegativeLength { length } => {
                format!(
                    "cannot use a negative integer, {}, for a value representing length",
                    length
                )
            }

            ErrorKind::UnmergeableInherit { name } => {
                format!(
                    "cannot merge a nested attribute set into the inherited entry '{}'",
                    name
                )
            }

            ErrorKind::UnmergeableValue => {
                "nested attribute sets or keys can only be merged with literal attribute sets"
                    .into()
            }

            ErrorKind::ReadFileError { path, error } => {
                format!(
                    "failed to read file '{}': {}",
                    path.to_string_lossy(),
                    error
                )
            }

            ErrorKind::ImportParseError { errors, path } => {
                format!(
                    "{} parse errors occured while importing '{}'",
                    errors.len(),
                    path.to_string_lossy()
                )
            }

            ErrorKind::ImportCompilerError { errors, path } => {
                // TODO: chain display of these errors, though this is
                // probably not the right place for that (should
                // branch into a more elaborate diagnostic() call
                // below).
                format!(
                    "{} errors occured while importing '{}'",
                    errors.len(),
                    path.to_string_lossy()
                )
            }

            ErrorKind::NotImplemented(feature) => {
                format!("feature not yet implemented in Tvix: {}", feature)
            }
        }
    }

    /// Return the unique error code for this variant which can be
    /// used to refer users to documentation.
    fn code(&self) -> &'static str {
        match self.kind {
            ErrorKind::Throw(_) => "E001",
            ErrorKind::Abort(_) => "E002",
            ErrorKind::AssertionFailed => "E003",
            ErrorKind::InvalidAttributeName { .. } => "E004",
            ErrorKind::AttributeNotFound { .. } => "E005",
            ErrorKind::TypeError { .. } => "E006",
            ErrorKind::Incomparable { .. } => "E007",
            ErrorKind::PathResolution(_) => "E008",
            ErrorKind::DynamicKeyInScope(_) => "E009",
            ErrorKind::UnknownStaticVariable => "E010",
            ErrorKind::UnknownDynamicVariable(_) => "E011",
            ErrorKind::VariableAlreadyDefined(_) => "E012",
            ErrorKind::NotCallable(_) => "E013",
            ErrorKind::InfiniteRecursion => "E014",
            ErrorKind::ParseErrors(_) => "E015",
            ErrorKind::DuplicateAttrsKey { .. } => "E016",
            ErrorKind::NotCoercibleToString { .. } => "E018",
            ErrorKind::IndexOutOfBounds { .. } => "E019",
            ErrorKind::NotAnAbsolutePath(_) => "E020",
            ErrorKind::ParseIntError(_) => "E021",
            ErrorKind::NegativeLength { .. } => "E022",
            ErrorKind::TailEmptyList { .. } => "E023",
            ErrorKind::UnmergeableInherit { .. } => "E024",
            ErrorKind::UnmergeableValue => "E025",
            ErrorKind::ReadFileError { .. } => "E026",
            ErrorKind::ImportParseError { .. } => "E027",
            ErrorKind::ImportCompilerError { .. } => "E028",

            // Placeholder error while Tvix is under construction.
            ErrorKind::NotImplemented(_) => "E999",

            // TODO: thunk force errors should yield a chained
            // diagnostic, but until then we just forward the error
            // code from the inner error.
            //
            // The error code for thunk forces is E017.
            ErrorKind::ThunkForce(ref err) => err.code(),
        }
    }

    fn diagnostic(&self) -> Diagnostic {
        let span_label = SpanLabel {
            label: self.span_label(),
            span: self.span,
            style: SpanStyle::Primary,
        };

        Diagnostic {
            level: Level::Error,
            message: self.message(),
            spans: vec![span_label],
            code: Some(self.code().into()),
        }
    }
}
