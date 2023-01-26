use std::{error, fmt::Display, rc::Rc};
use tvix_derivation::DerivationError;

#[derive(Debug)]
pub enum Error {
    // Errors related to derivation construction
    DuplicateOutput(String),
    ConflictingOutputTypes,
    DuplicateEnvVar(String),
    ShadowedOutput(String),
    InvalidDerivation(DerivationError),
    InvalidOutputHashMode(String),
    UnsupportedSRIAlgo(String),
    UnsupportedSRIMultiple(usize),
    InvalidSRIDigest(data_encoding::DecodeError),
    InvalidSRIString(ssri::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::DuplicateOutput(name) => {
                write!(f, "an output with the name '{name}' is already defined")
            }

            Error::ConflictingOutputTypes => write!(
                f,
                "fixed-output derivations can only have the default `out`-output"
            ),

            Error::DuplicateEnvVar(name) => write!(
                f,
                "the environment variable '{name}' has already been set in this derivation"
            ),
            Error::ShadowedOutput(name) => write!(
                f,
                "the environment variable '{name}' shadows the name of an output"
            ),
            Error::InvalidDerivation(error) => write!(f, "invalid derivation parameters: {error}"),

            Error::InvalidOutputHashMode(mode) => write!(
                f,
                "invalid output hash mode: '{mode}', only 'recursive' and 'flat` are supported"
            ),
            Error::UnsupportedSRIAlgo(algo) => {
                write!(f, "invalid sri algorithm: {algo}, only sha256 is supported")
            }
            Error::UnsupportedSRIMultiple(n) => {
                write!(
                    f,
                    "invalid number of sri hashes in string ({n}), only one hash is supported"
                )
            }
            Error::InvalidSRIDigest(err) => {
                write!(f, "invalid sri digest: {}", err)
            }
            Error::InvalidSRIString(err) => {
                write!(f, "failed to parse SRI string: {}", err)
            }
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
