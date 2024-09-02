use serde::Serialize;

/// These are the errors which can show up during a massive evaluation job.
#[derive(Debug, thiserror::Error, Serialize)]
pub enum EvaluationError {
    #[error("expecting `{0}` attribute in a derivation")]
    MissingAttribute(String),
    #[error("expecting an attribute set in a derivation, found `{0}`")]
    MalformedDerivation(String),
    #[error("shallow force failure")]
    ShallowForceFailure {
        /// Which attribute were we evaluating
        value_path: String,
        // error_pretty_fmt: String,
    },
    #[error("deep force failure")]
    DeepForceFailure {
        /// Which attribute were we evaluating
        value_path: String,
        // error_pretty_fmt: String,
    },
}
