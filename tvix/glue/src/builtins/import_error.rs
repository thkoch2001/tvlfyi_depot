//! Contains [crate::builtins::ImportError].
use std::rc::Rc;
use thiserror::Error;

/// Errors related to `builtins.path` and `builtins.filterSource`,
/// a.k.a. "importing" builtins.
#[derive(Debug, Error)]
pub enum Error {
    #[error("non-file '{0}' cannot be imported in 'flat' mode")]
    FlatImportOfNonFile(String),
    #[error("hash mismatch at ingestion of '{0}', expected: '{1}', got: '{2}'")]
    HashMismatch(String, String, String),
}

impl From<Error> for tvix_eval::ErrorKind {
    fn from(err: Error) -> Self {
        tvix_eval::ErrorKind::TvixError(Rc::new(err))
    }
}
