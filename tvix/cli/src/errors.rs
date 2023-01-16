use std::{error, fmt::Display, rc::Rc};

#[derive(Debug)]
pub enum Error {
    // Errors related to derivation construction
    InvalidOutputName(String),
    DuplicateOutput(String),
    ConflictingOutputTypes,
    MissingFODFields,
    DuplicateEnvVar(String),
    ShadowedOutput(String),

    // This error variant is constructed from the untyped
    // //tvix/derivation errors.
    // TODO: add typed variants for these
    InvalidDerivation(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidOutputName(name) => write!(f, "invalid derivation output name: {name}"),

            Error::DuplicateOutput(name) => {
                write!(f, "an output with the name '{name}' is already defined")
            }

            Error::ConflictingOutputTypes => write!(
                f,
                "fixed-output derivations can only have the default `out`-output"
            ),

            Error::MissingFODFields => write!(f, "both `outputHash` and `outputHashAlgo` must be specified for fixed-output derivations"),

            Error::DuplicateEnvVar(name) => write!(f, "the environment variable '{name}' has already been set in this derivation"),
            Error::ShadowedOutput(name) => write!(f, "the environment variable '{name}' shadows the name of an output"),
            Error::InvalidDerivation(error) => write!(f, "invalid derivation parameters: {error}"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl From<Error> for tvix_eval::ErrorKind {
    fn from(err: Error) -> Self {
        tvix_eval::ErrorKind::TvixError(Rc::new(err))
    }
}
