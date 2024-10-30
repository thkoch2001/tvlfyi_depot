use std::error;
use std::io;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::Utf8Error;
use std::string::FromUtf8Error;
use std::sync::Arc;
use std::{fmt::Debug, fmt::Display, num::ParseIntError};

use codemap::{File, Span};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use smol_str::SmolStr;

use crate::spans::ToSpan;
use crate::value::{CoercionKind, NixString};
use crate::{SourceCode, Value};

/// "CatchableErrorKind" errors -- those which can be detected by
/// `builtins.tryEval`.
///
/// Note: this type is deliberately *not* incorporated as a variant
/// of ErrorKind, because then Result<Value,ErrorKind> would have
/// redundant representations for catchable errors, which would make
/// it too easy to handle errors incorrectly:
///
///   - Ok(Value::Catchable(cek))
///   - Err(ErrorKind::ThisVariantDoesNotExist(cek))
///
/// Because CatchableErrorKind is not a variant of ErrorKind, you
/// will often see functions which return a type like:
///
///   Result<Result<T,CatchableErrorKind>,ErrorKind>
///
/// ... where T is any type other than Value.  This is unfortunate,
/// because Rust's magic `?`-syntax does not work on nested Result
/// values like this.
// TODO(amjoseph): investigate result<T,Either<CatchableErrorKind,ErrorKind>>
#[derive(thiserror::Error, Clone, Debug)]
pub enum CatchableErrorKind {
    #[error("error thrown: {0}")]
    Throw(Box<str>),

    #[error("assertion failed")]
    AssertionFailed,

    #[error("feature {0} is not implemented yet")]
    UnimplementedFeature(Box<str>),

    /// Resolving a user-supplied angle brackets path literal failed in some way.
    #[error("Nix path entry could not be resolved: {0}")]
    NixPathResolution(Box<str>),
}

#[derive(thiserror::Error, Clone, Debug)]
pub enum ErrorKind {
    /// These are user-generated errors through builtins.
    #[error("evaluation aborted: {0}")]
    Abort(String),

    #[error("division by zero")]
    DivisionByZero,

    #[error("attribute key '{key}' already defined")]
    DuplicateAttrsKey { key: String },

    /// Attempted to specify an invalid key type (e.g. integer) in a
    /// dynamic attribute name.
    #[error(
        "found attribute name '{0}' of type '{}', but attribute names must be strings",
        .0.type_of()
    )]
    InvalidAttributeName(Value),

    #[error("attribute with name '{name}' could not be found in the set")]
    AttributeNotFound { name: String },

    /// Attempted to index into a list beyond its boundaries.
    #[error("list index '{index}' is out of bounds")]
    IndexOutOfBounds { index: i64 },

    /// Attempted to call `builtins.tail` on an empty list.
    #[error("'tail' called on an empty list")]
    TailEmptyList,

    #[error("expected value of type '{expected}', but found a '{actual}'")]
    TypeError {
        expected: &'static str,
        actual: &'static str,
    },

    #[error("can not compare a {lhs} with a {rhs}")]
    Incomparable {
        lhs: &'static str,
        rhs: &'static str,
    },

    /// Resolving a user-supplied relative or home-relative path literal failed in some way.
    #[error("could not resolve path: {0}")]
    RelativePathResolution(String),

    /// Dynamic keys are not allowed in some scopes.
    #[error("dynamically evaluated keys are not allowed in {0}")]
    DynamicKeyInScope(&'static str),

    /// Unknown variable in statically known scope.
    #[error("variable not found")]
    UnknownStaticVariable,

    /// Unknown variable in dynamic scope (with, rec, ...).
    #[error(
        r#"variable '{0}' could not be found

Note that this occured within a `with`-expression. The problem may be related
to a missing value in the attribute set(s) included via `with`."#
    )]
    UnknownDynamicVariable(String),

    /// User is defining the same variable twice at the same depth.
    #[error("variable has already been defined")]
    VariableAlreadyDefined(Option<Span>),

    /// Attempt to call something that is not callable.
    #[error("only functions and builtins can be called, but this is a '{0}'")]
    NotCallable(&'static str),

    /// Infinite recursion encountered while forcing thunks.
    #[error("infinite recursion encountered")]
    InfiniteRecursion {
        first_force: Span,
        suspended_at: Option<Span>,
        content_span: Option<Span>,
    },

    // Errors themselves ignored here & handled in Self::spans instead
    #[error("failed to parse Nix code:")]
    ParseErrors(Vec<rnix::parser::ParseError>),

    /// An error occured while executing some native code (e.g. a
    /// builtin), and needs to be chained up.
    #[error("while evaluating this as native code ({gen_type})")]
    NativeError {
        gen_type: &'static str,
        err: Box<Error>,
    },

    /// An error occured while executing Tvix bytecode, but needs to
    /// be chained up.
    #[error("while evaluating this Nix code")]
    BytecodeError(Box<Error>),

    /// Given type can't be coerced to a string in the respective context
    #[error("cannot ({}) coerce {from} to a string{}", 
        (if .kind.strong { "strongly" } else { "weakly" }),
        (if *.from == "set" {
            ", missing a `__toString` or `outPath` attribute"
        } else {
            ""
        })
    )]
    NotCoercibleToString {
        from: &'static str,
        kind: CoercionKind,
    },

    /// The given string doesn't represent an absolute path
    #[error("string '{}' does not represent an absolute path", .0.to_string_lossy())]
    NotAnAbsolutePath(PathBuf),

    /// An error occurred when parsing an integer
    #[error("invalid integer: {0}")]
    ParseIntError(ParseIntError),

    // Errors specific to nested attribute sets and merges thereof.
    /// Nested attributes can not be merged with an inherited value.
    #[error("cannot merge a nested attribute set into the inherited entry '{name}'")]
    UnmergeableInherit { name: SmolStr },

    /// Nested attributes can not be merged with values that are not
    /// literal attribute sets.
    #[error("nested attribute sets or keys can only be merged with literal attribute sets")]
    UnmergeableValue,

    // Errors themselves ignored here & handled in Self::spans instead
    /// Parse errors occured while importing a file.
    #[error("parse errors occured while importing '{}'", .path.to_string_lossy())]
    ImportParseError {
        path: PathBuf,
        file: Arc<File>,
        errors: Vec<rnix::parser::ParseError>,
    },

    /// Compilation errors occured while importing a file.
    #[error("compiler errors occured while importing '{}'", .path.to_string_lossy())]
    ImportCompilerError { path: PathBuf, errors: Vec<Error> },

    /// I/O errors
    #[error("I/O error: {}",
        ({
            let mut msg = String::new();

            if let Some(path) = .path {
                msg.push_str(&format!("{}: ", path.display()));
            }

            msg.push_str(&.error.to_string());

            msg
        })
    )]
    IO {
        path: Option<PathBuf>,
        error: Rc<io::Error>,
    },

    /// Errors parsing JSON, or serializing as JSON.
    #[error("Error converting JSON to a Nix value or back: {0}")]
    JsonError(String),

    /// Nix value that can not be serialised to JSON.
    #[error("a {0} cannot be converted to JSON")]
    NotSerialisableToJson(&'static str),

    /// Errors converting TOML to a value
    #[error("Error converting TOML to a Nix value: {0}")]
    FromTomlError(String),

    /// An unexpected argument was supplied to a builtin
    #[error("Unexpected agrument `{0}` passed to builtin")]
    UnexpectedArgumentBuiltin(NixString),

    /// An unexpected argument was supplied to a function that takes formal parameters
    #[error("Unexpected argument `{arg}` supplied to function")]
    UnexpectedArgumentFormals { arg: NixString, formals_span: Span },

    /// Invalid UTF-8 was encoutered somewhere
    #[error("Invalid UTF-8 in string")]
    Utf8,

    #[error("Invalid hash: {0}")]
    InvalidHash(String),

    /// Variant for errors that bubble up to eval from other Tvix
    /// components.
    #[error("{0}")]
    TvixError(Rc<dyn error::Error>),

    /// Variant for code paths that are known bugs in Tvix (usually
    /// issues with the compiler/VM interaction).
    #[error("{}",
        ({
            let mut disp = format!("Tvix bug: {}", .msg);

            if let Some(metadata) = .metadata {
                disp.push_str(&format!("; metadata: {:?}", metadata));
            }

            disp
        })
    )]
    TvixBug {
        msg: &'static str,
        metadata: Option<Rc<dyn Debug>>,
    },

    /// Tvix internal warning for features triggered by users that are
    /// not actually implemented yet, and without which eval can not
    /// proceed.
    #[error("feature not yet implemented in Tvix: {0}")]
    NotImplemented(&'static str),

    /// Internal variant which should disappear during error construction.
    #[error("internal ErrorKind::WithContext variant leaked")]
    WithContext {
        context: String,
        underlying: Box<ErrorKind>,
    },

    /// Unexpected context string
    #[error("unexpected context string")]
    UnexpectedContext,

    /// Top-level evaluation result was a catchable Nix error, and
    /// should fail the evaluation.
    ///
    /// This variant **must** only be used at the top-level of
    /// tvix-eval when returning a result to the user, never inside of
    /// eval code.
    #[error("{0}")]
    CatchableError(CatchableErrorKind),

    /// Invalid hash type specified, must be one of "md5", "sha1", "sha256"
    /// or "sha512"
    #[error("unknown hash type '{0}'")]
    UnknownHashType(String),
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match &self.kind {
            ErrorKind::NativeError { err, .. } | ErrorKind::BytecodeError(err) => err.source(),
            ErrorKind::ParseErrors(err) => err.first().map(|e| e as &dyn error::Error),
            ErrorKind::ParseIntError(err) => Some(err),
            ErrorKind::ImportParseError { errors, .. } => {
                errors.first().map(|e| e as &dyn error::Error)
            }
            ErrorKind::ImportCompilerError { errors, .. } => {
                errors.first().map(|e| e as &dyn error::Error)
            }
            ErrorKind::IO { error, .. } => Some(error.as_ref()),
            ErrorKind::TvixError(error) => Some(error.as_ref()),
            _ => None,
        }
    }
}

impl From<ParseIntError> for ErrorKind {
    fn from(e: ParseIntError) -> Self {
        Self::ParseIntError(e)
    }
}

impl From<Utf8Error> for ErrorKind {
    fn from(_: Utf8Error) -> Self {
        Self::NotImplemented("FromUtf8Error not handled: https://b.tvl.fyi/issues/189")
    }
}

impl From<FromUtf8Error> for ErrorKind {
    fn from(_: FromUtf8Error) -> Self {
        Self::NotImplemented("FromUtf8Error not handled: https://b.tvl.fyi/issues/189")
    }
}

impl From<bstr::Utf8Error> for ErrorKind {
    fn from(_: bstr::Utf8Error) -> Self {
        Self::Utf8
    }
}

impl From<bstr::FromUtf8Error> for ErrorKind {
    fn from(_value: bstr::FromUtf8Error) -> Self {
        Self::Utf8
    }
}

impl From<io::Error> for ErrorKind {
    fn from(e: io::Error) -> Self {
        ErrorKind::IO {
            path: None,
            error: Rc::new(e),
        }
    }
}

impl From<serde_json::Error> for ErrorKind {
    fn from(err: serde_json::Error) -> Self {
        // Can't just put the `serde_json::Error` in the ErrorKind since it doesn't impl `Clone`
        Self::JsonError(err.to_string())
    }
}

impl From<toml::de::Error> for ErrorKind {
    fn from(err: toml::de::Error) -> Self {
        Self::FromTomlError(format!("error in TOML serialization: {err}"))
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
    pub contexts: Vec<String>,
    pub source: SourceCode,
}

impl Error {
    pub fn new(mut kind: ErrorKind, span: Span, source: SourceCode) -> Self {
        let mut contexts = vec![];
        while let ErrorKind::WithContext {
            context,
            underlying,
        } = kind
        {
            kind = *underlying;
            contexts.push(context);
        }

        Error {
            kind,
            span,
            contexts,
            source,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub type EvalResult<T> = Result<T, Error>;

/// Human-readable names for rnix syntaxes.
fn name_for_syntax(syntax: &rnix::SyntaxKind) -> &'static str {
    match syntax {
        rnix::SyntaxKind::TOKEN_COMMENT => "a comment",
        rnix::SyntaxKind::TOKEN_WHITESPACE => "whitespace",
        rnix::SyntaxKind::TOKEN_ASSERT => "`assert`-keyword",
        rnix::SyntaxKind::TOKEN_ELSE => "`else`-keyword",
        rnix::SyntaxKind::TOKEN_IN => "`in`-keyword",
        rnix::SyntaxKind::TOKEN_IF => "`if`-keyword",
        rnix::SyntaxKind::TOKEN_INHERIT => "`inherit`-keyword",
        rnix::SyntaxKind::TOKEN_LET => "`let`-keyword",
        rnix::SyntaxKind::TOKEN_OR => "`or`-keyword",
        rnix::SyntaxKind::TOKEN_REC => "`rec`-keyword",
        rnix::SyntaxKind::TOKEN_THEN => "`then`-keyword",
        rnix::SyntaxKind::TOKEN_WITH => "`with`-keyword",
        rnix::SyntaxKind::TOKEN_L_BRACE => "{",
        rnix::SyntaxKind::TOKEN_R_BRACE => "}",
        rnix::SyntaxKind::TOKEN_L_BRACK => "[",
        rnix::SyntaxKind::TOKEN_R_BRACK => "]",
        rnix::SyntaxKind::TOKEN_ASSIGN => "=",
        rnix::SyntaxKind::TOKEN_AT => "@",
        rnix::SyntaxKind::TOKEN_COLON => ":",
        rnix::SyntaxKind::TOKEN_COMMA => "`,`",
        rnix::SyntaxKind::TOKEN_DOT => ".",
        rnix::SyntaxKind::TOKEN_ELLIPSIS => "...",
        rnix::SyntaxKind::TOKEN_QUESTION => "?",
        rnix::SyntaxKind::TOKEN_SEMICOLON => ";",
        rnix::SyntaxKind::TOKEN_L_PAREN => "(",
        rnix::SyntaxKind::TOKEN_R_PAREN => ")",
        rnix::SyntaxKind::TOKEN_CONCAT => "++",
        rnix::SyntaxKind::TOKEN_INVERT => "!",
        rnix::SyntaxKind::TOKEN_UPDATE => "//",
        rnix::SyntaxKind::TOKEN_ADD => "+",
        rnix::SyntaxKind::TOKEN_SUB => "-",
        rnix::SyntaxKind::TOKEN_MUL => "*",
        rnix::SyntaxKind::TOKEN_DIV => "/",
        rnix::SyntaxKind::TOKEN_AND_AND => "&&",
        rnix::SyntaxKind::TOKEN_EQUAL => "==",
        rnix::SyntaxKind::TOKEN_IMPLICATION => "->",
        rnix::SyntaxKind::TOKEN_LESS => "<",
        rnix::SyntaxKind::TOKEN_LESS_OR_EQ => "<=",
        rnix::SyntaxKind::TOKEN_MORE => ">",
        rnix::SyntaxKind::TOKEN_MORE_OR_EQ => ">=",
        rnix::SyntaxKind::TOKEN_NOT_EQUAL => "!=",
        rnix::SyntaxKind::TOKEN_OR_OR => "||",
        rnix::SyntaxKind::TOKEN_FLOAT => "a float",
        rnix::SyntaxKind::TOKEN_IDENT => "an identifier",
        rnix::SyntaxKind::TOKEN_INTEGER => "an integer",
        rnix::SyntaxKind::TOKEN_INTERPOL_END => "}",
        rnix::SyntaxKind::TOKEN_INTERPOL_START => "${",
        rnix::SyntaxKind::TOKEN_PATH => "a path",
        rnix::SyntaxKind::TOKEN_URI => "a literal URI",
        rnix::SyntaxKind::TOKEN_STRING_CONTENT => "content of a string",
        rnix::SyntaxKind::TOKEN_STRING_END => "\"",
        rnix::SyntaxKind::TOKEN_STRING_START => "\"",

        rnix::SyntaxKind::NODE_APPLY => "a function application",
        rnix::SyntaxKind::NODE_ASSERT => "an assertion",
        rnix::SyntaxKind::NODE_ATTRPATH => "an attribute path",
        rnix::SyntaxKind::NODE_DYNAMIC => "a dynamic identifier",

        rnix::SyntaxKind::NODE_IDENT => "an identifier",
        rnix::SyntaxKind::NODE_IF_ELSE => "an `if`-expression",
        rnix::SyntaxKind::NODE_SELECT => "a `select`-expression",
        rnix::SyntaxKind::NODE_INHERIT => "inherited values",
        rnix::SyntaxKind::NODE_INHERIT_FROM => "inherited values",
        rnix::SyntaxKind::NODE_STRING => "a string",
        rnix::SyntaxKind::NODE_INTERPOL => "an interpolation",
        rnix::SyntaxKind::NODE_LAMBDA => "a function",
        rnix::SyntaxKind::NODE_IDENT_PARAM => "a function parameter",
        rnix::SyntaxKind::NODE_LEGACY_LET => "a legacy `let`-expression",
        rnix::SyntaxKind::NODE_LET_IN => "a `let`-expression",
        rnix::SyntaxKind::NODE_LIST => "a list",
        rnix::SyntaxKind::NODE_BIN_OP => "a binary operator",
        rnix::SyntaxKind::NODE_PAREN => "a parenthesised expression",
        rnix::SyntaxKind::NODE_PATTERN => "a function argument pattern",
        rnix::SyntaxKind::NODE_PAT_BIND => "an argument pattern binding",
        rnix::SyntaxKind::NODE_PAT_ENTRY => "an argument pattern entry",
        rnix::SyntaxKind::NODE_ROOT => "a Nix expression",
        rnix::SyntaxKind::NODE_ATTR_SET => "an attribute set",
        rnix::SyntaxKind::NODE_ATTRPATH_VALUE => "an attribute set entry",
        rnix::SyntaxKind::NODE_UNARY_OP => "a unary operator",
        rnix::SyntaxKind::NODE_LITERAL => "a literal value",
        rnix::SyntaxKind::NODE_WITH => "a `with`-expression",
        rnix::SyntaxKind::NODE_PATH => "a path",
        rnix::SyntaxKind::NODE_HAS_ATTR => "`?`-operator",

        // TODO(tazjin): unsure what these variants are, lets crash!
        rnix::SyntaxKind::NODE_ERROR => todo!("NODE_ERROR found, tell tazjin!"),
        rnix::SyntaxKind::TOKEN_ERROR => todo!("TOKEN_ERROR found, tell tazjin!"),
        _ => todo!(),
    }
}

/// Construct the string representation for a list of expected parser tokens.
fn expected_syntax(one_of: &[rnix::SyntaxKind]) -> String {
    match one_of.len() {
        0 => "nothing".into(),
        1 => format!("'{}'", name_for_syntax(&one_of[0])),
        _ => {
            let mut out: String = "one of: ".into();
            let end = one_of.len() - 1;

            for (idx, item) in one_of.iter().enumerate() {
                if idx != 0 {
                    out.push_str(", ");
                } else if idx == end {
                    out.push_str(", or ");
                };

                out.push_str(name_for_syntax(item));
            }

            out
        }
    }
}

/// Process a list of parse errors into a set of span labels, annotating parse
/// errors.
fn spans_for_parse_errors(file: &File, errors: &[rnix::parser::ParseError]) -> Vec<SpanLabel> {
    // rnix has a tendency to emit some identical errors more than once, but
    // they do not enhance the user experience necessarily, so we filter them
    // out
    let mut had_eof = false;

    errors
        .iter()
        .enumerate()
        .filter_map(|(idx, err)| {
            let (span, label): (Span, String) = match err {
                rnix::parser::ParseError::Unexpected(range) => (
                    range.span_for(file),
                    "found an unexpected syntax element here".into(),
                ),

                rnix::parser::ParseError::UnexpectedExtra(range) => (
                    range.span_for(file),
                    "found unexpected extra elements at the root of the expression".into(),
                ),

                rnix::parser::ParseError::UnexpectedWanted(found, range, wanted) => {
                    let span = range.span_for(file);
                    (
                        span,
                        format!(
                            "found '{}', but expected {}",
                            name_for_syntax(found),
                            expected_syntax(wanted),
                        ),
                    )
                }

                rnix::parser::ParseError::UnexpectedEOF => {
                    if had_eof {
                        return None;
                    }

                    had_eof = true;

                    (
                        file.span,
                        "code ended unexpectedly while the parser still expected more".into(),
                    )
                }

                rnix::parser::ParseError::UnexpectedEOFWanted(wanted) => {
                    had_eof = true;

                    (
                        file.span,
                        format!(
                            "code ended unexpectedly, but wanted {}",
                            expected_syntax(wanted)
                        ),
                    )
                }

                rnix::parser::ParseError::DuplicatedArgs(range, name) => (
                    range.span_for(file),
                    format!(
                        "the function argument pattern '{}' was bound more than once",
                        name
                    ),
                ),

                rnix::parser::ParseError::RecursionLimitExceeded => (
                    file.span,
                    "this code exceeds the parser's recursion limit, please report a Tvix bug"
                        .to_string(),
                ),

                // TODO: can rnix even still throw this? it's semantic!
                rnix::parser::ParseError::UnexpectedDoubleBind(range) => (
                    range.span_for(file),
                    "this pattern was bound more than once".into(),
                ),

                // The error enum is marked as `#[non_exhaustive]` in rnix,
                // which disables the compiler error for missing a variant. This
                // feature makes it possible for users to miss critical updates
                // of enum variants for a more exciting runtime experience.
                new => todo!("new parse error variant: {}", new),
            };

            Some(SpanLabel {
                span,
                label: Some(label),
                style: if idx == 0 {
                    SpanStyle::Primary
                } else {
                    SpanStyle::Secondary
                },
            })
        })
        .collect()
}

impl Error {
    pub fn fancy_format_str(&self) -> String {
        let mut out = vec![];
        Emitter::vec(&mut out, Some(&*self.source.codemap())).emit(&self.diagnostics());
        String::from_utf8_lossy(&out).to_string()
    }

    /// Render a fancy, human-readable output of this error and print
    /// it to stderr.
    pub fn fancy_format_stderr(&self) {
        Emitter::stderr(ColorConfig::Auto, Some(&*self.source.codemap())).emit(&self.diagnostics());
    }

    /// Create the optional span label displayed as an annotation on
    /// the underlined span of the error.
    fn span_label(&self) -> Option<String> {
        let label = match &self.kind {
            ErrorKind::DuplicateAttrsKey { .. } => "in this attribute set",
            ErrorKind::InvalidAttributeName(_) => "in this attribute set",
            ErrorKind::RelativePathResolution(_) => "in this path literal",
            ErrorKind::UnexpectedArgumentBuiltin { .. } => "while calling this builtin",
            ErrorKind::UnexpectedArgumentFormals { .. } => "in this function call",
            ErrorKind::UnexpectedContext => "in this string",

            // The spans for some errors don't have any more descriptive stuff
            // in them, or we don't utilise it yet.
            ErrorKind::Abort(_)
            | ErrorKind::AttributeNotFound { .. }
            | ErrorKind::IndexOutOfBounds { .. }
            | ErrorKind::TailEmptyList
            | ErrorKind::TypeError { .. }
            | ErrorKind::Incomparable { .. }
            | ErrorKind::DivisionByZero
            | ErrorKind::DynamicKeyInScope(_)
            | ErrorKind::UnknownStaticVariable
            | ErrorKind::UnknownDynamicVariable(_)
            | ErrorKind::VariableAlreadyDefined(_)
            | ErrorKind::NotCallable(_)
            | ErrorKind::InfiniteRecursion { .. }
            | ErrorKind::ParseErrors(_)
            | ErrorKind::NativeError { .. }
            | ErrorKind::BytecodeError(_)
            | ErrorKind::NotCoercibleToString { .. }
            | ErrorKind::NotAnAbsolutePath(_)
            | ErrorKind::ParseIntError(_)
            | ErrorKind::UnmergeableInherit { .. }
            | ErrorKind::UnmergeableValue
            | ErrorKind::ImportParseError { .. }
            | ErrorKind::ImportCompilerError { .. }
            | ErrorKind::IO { .. }
            | ErrorKind::JsonError(_)
            | ErrorKind::NotSerialisableToJson(_)
            | ErrorKind::FromTomlError(_)
            | ErrorKind::Utf8
            | ErrorKind::TvixError(_)
            | ErrorKind::TvixBug { .. }
            | ErrorKind::NotImplemented(_)
            | ErrorKind::WithContext { .. }
            | ErrorKind::UnknownHashType(_)
            | ErrorKind::InvalidHash(_)
            | ErrorKind::CatchableError(_) => return None,
        };

        Some(label.into())
    }

    /// Return the unique error code for this variant which can be
    /// used to refer users to documentation.
    fn code(&self) -> &'static str {
        match self.kind {
            ErrorKind::CatchableError(CatchableErrorKind::Throw(_)) => "E001",
            ErrorKind::Abort(_) => "E002",
            ErrorKind::CatchableError(CatchableErrorKind::AssertionFailed) => "E003",
            ErrorKind::InvalidAttributeName { .. } => "E004",
            ErrorKind::AttributeNotFound { .. } => "E005",
            ErrorKind::TypeError { .. } => "E006",
            ErrorKind::Incomparable { .. } => "E007",
            ErrorKind::CatchableError(CatchableErrorKind::NixPathResolution(_)) => "E008",
            ErrorKind::DynamicKeyInScope(_) => "E009",
            ErrorKind::UnknownStaticVariable => "E010",
            ErrorKind::UnknownDynamicVariable(_) => "E011",
            ErrorKind::VariableAlreadyDefined(_) => "E012",
            ErrorKind::NotCallable(_) => "E013",
            ErrorKind::InfiniteRecursion { .. } => "E014",
            ErrorKind::ParseErrors(_) => "E015",
            ErrorKind::DuplicateAttrsKey { .. } => "E016",
            ErrorKind::NotCoercibleToString { .. } => "E018",
            ErrorKind::IndexOutOfBounds { .. } => "E019",
            ErrorKind::NotAnAbsolutePath(_) => "E020",
            ErrorKind::ParseIntError(_) => "E021",
            ErrorKind::TailEmptyList { .. } => "E023",
            ErrorKind::UnmergeableInherit { .. } => "E024",
            ErrorKind::UnmergeableValue => "E025",
            ErrorKind::ImportParseError { .. } => "E027",
            ErrorKind::ImportCompilerError { .. } => "E028",
            ErrorKind::IO { .. } => "E029",
            ErrorKind::JsonError { .. } => "E030",
            ErrorKind::UnexpectedArgumentFormals { .. } => "E031",
            ErrorKind::RelativePathResolution(_) => "E032",
            ErrorKind::DivisionByZero => "E033",
            ErrorKind::FromTomlError(_) => "E035",
            ErrorKind::NotSerialisableToJson(_) => "E036",
            ErrorKind::UnexpectedContext => "E037",
            ErrorKind::Utf8 => "E038",
            ErrorKind::UnknownHashType(_) => "E039",
            ErrorKind::UnexpectedArgumentBuiltin { .. } => "E040",
            ErrorKind::InvalidHash(_) => "E041",

            // Special error code for errors from other Tvix
            // components. We may want to introduce a code namespacing
            // system to have these errors pass codes through.
            ErrorKind::TvixError(_) => "E997",

            // Special error code that is not part of the normal
            // ordering.
            ErrorKind::TvixBug { .. } => "E998",

            // Placeholder error while Tvix is under construction.
            ErrorKind::CatchableError(CatchableErrorKind::UnimplementedFeature(_))
            | ErrorKind::NotImplemented(_) => "E999",

            // Chained errors should yield the code of the innermost
            // error.
            ErrorKind::NativeError { ref err, .. } | ErrorKind::BytecodeError(ref err) => {
                err.code()
            }

            ErrorKind::WithContext { .. } => {
                panic!("internal ErrorKind::WithContext variant leaked")
            }
        }
    }

    fn spans(&self) -> Vec<SpanLabel> {
        let mut spans = match &self.kind {
            ErrorKind::ImportParseError { errors, file, .. } => {
                spans_for_parse_errors(file, errors)
            }

            ErrorKind::ParseErrors(errors) => {
                let file = self.source.get_file(self.span);
                spans_for_parse_errors(&file, errors)
            }

            ErrorKind::UnexpectedArgumentFormals { formals_span, .. } => {
                vec![
                    SpanLabel {
                        label: self.span_label(),
                        span: self.span,
                        style: SpanStyle::Primary,
                    },
                    SpanLabel {
                        label: Some("the accepted arguments".into()),
                        span: *formals_span,
                        style: SpanStyle::Secondary,
                    },
                ]
            }

            ErrorKind::InfiniteRecursion {
                first_force,
                suspended_at,
                content_span,
            } => {
                let mut spans = vec![];

                if let Some(content_span) = content_span {
                    spans.push(SpanLabel {
                        label: Some("this lazily-evaluated code".into()),
                        span: *content_span,
                        style: SpanStyle::Secondary,
                    })
                }

                if let Some(suspended_at) = suspended_at {
                    spans.push(SpanLabel {
                        label: Some("which was instantiated here".into()),
                        span: *suspended_at,
                        style: SpanStyle::Secondary,
                    })
                }

                spans.push(SpanLabel {
                    label: Some("was first requested to be evaluated here".into()),
                    span: *first_force,
                    style: SpanStyle::Secondary,
                });

                spans.push(SpanLabel {
                    label: Some("but then requested again here during its own evaluation".into()),
                    span: self.span,
                    style: SpanStyle::Primary,
                });

                spans
            }

            // All other errors pretty much have the same shape.
            _ => {
                vec![SpanLabel {
                    label: self.span_label(),
                    span: self.span,
                    style: SpanStyle::Primary,
                }]
            }
        };

        for ctx in &self.contexts {
            spans.push(SpanLabel {
                label: Some(format!("while {}", ctx)),
                span: self.span,
                style: SpanStyle::Secondary,
            });
        }

        spans
    }

    /// Create the primary diagnostic for a given error.
    fn diagnostic(&self) -> Diagnostic {
        Diagnostic {
            level: Level::Error,
            message: self.to_string(),
            spans: self.spans(),
            code: Some(self.code().into()),
        }
    }

    /// Return the primary diagnostic and all further associated diagnostics (if
    /// any) of an error.
    fn diagnostics(&self) -> Vec<Diagnostic> {
        match &self.kind {
            ErrorKind::ImportCompilerError { errors, .. } => {
                let mut out = vec![self.diagnostic()];
                out.extend(errors.iter().map(|e| e.diagnostic()));
                out
            }

            // When encountering either of these error kinds, we are dealing
            // with the top of an error chain.
            //
            // An error chain creates a list of diagnostics which provide trace
            // information.
            //
            // We don't know how deep this chain is, so we avoid recursing in
            // this function while unrolling the chain.
            ErrorKind::NativeError { err: next, .. } | ErrorKind::BytecodeError(next) => {
                // Accumulated diagnostics to return.
                let mut diagnostics: Vec<Diagnostic> = vec![];

                // The next (inner) error to add to the diagnostics, after this
                // one.
                let mut next = *next.clone();

                // Diagnostic message for *this* error.
                let mut this_message = self.to_string();

                // Primary span for *this* error.
                let mut this_span = self.span;

                // Diagnostic spans for *this* error.
                let mut this_spans = self.spans();

                loop {
                    if is_new_span(
                        this_span,
                        diagnostics.last().and_then(|last| last.spans.last()),
                    ) {
                        diagnostics.push(Diagnostic {
                            level: Level::Note,
                            message: this_message,
                            spans: this_spans,
                            code: None, // only the top-level error has one
                        });
                    }

                    this_message = next.to_string();
                    this_span = next.span;
                    this_spans = next.spans();

                    match next.kind {
                        ErrorKind::NativeError { err: inner, .. }
                        | ErrorKind::BytecodeError(inner) => {
                            next = *inner;
                            continue;
                        }
                        _ => {
                            diagnostics.extend(next.diagnostics());
                            break;
                        }
                    }
                }

                diagnostics
            }

            _ => vec![self.diagnostic()],
        }
    }
}

// Check if this error is in a different span from its immediate ancestor.
fn is_new_span(this_span: Span, parent: Option<&SpanLabel>) -> bool {
    match parent {
        None => true,
        Some(parent) => parent.span != this_span,
    }
}

// Convenience methods to add context on other types.
pub trait AddContext {
    /// Add context to the error-carrying type.
    fn context<S: Into<String>>(self, ctx: S) -> Self;
}

impl AddContext for ErrorKind {
    fn context<S: Into<String>>(self, ctx: S) -> Self {
        ErrorKind::WithContext {
            context: ctx.into(),
            underlying: Box::new(self),
        }
    }
}

impl<T> AddContext for Result<T, ErrorKind> {
    fn context<S: Into<String>>(self, ctx: S) -> Self {
        self.map_err(|kind| kind.context(ctx))
    }
}

impl<T> AddContext for Result<T, Error> {
    fn context<S: Into<String>>(self, ctx: S) -> Self {
        self.map_err(|err| Error {
            kind: err.kind.context(ctx),
            ..err
        })
    }
}
