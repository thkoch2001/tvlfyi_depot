//! This module defines error types and conversions for issue that can
//! occur while dealing with persisted state machines.

use std::result;
use std::fmt::Display;
use uuid::Uuid;

// errors to chain:
use serde_json::Error as JsonError;
use postgres::Error as PgError;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub context: Option<String>,
}

#[derive(Debug)]
pub enum ErrorKind {
    /// Errors occuring during JSON serialization of FSM types.
    Serialization(String),

    /// Errors occuring during communication with the database.
    Database(String),

    /// State machine could not be found.
    FSMNotFound(Uuid),

    /// Action could not be found.
    ActionNotFound(Uuid),
}

impl <E: Into<ErrorKind>> From<E> for Error {
    fn from(err: E) -> Error {
        Error {
            kind: err.into(),
            context: None,
        }
    }
}

impl From<JsonError> for ErrorKind {
    fn from(err: JsonError) -> ErrorKind {
        ErrorKind::Serialization(err.to_string())
    }
}

impl From<PgError> for ErrorKind {
    fn from(err: PgError) -> ErrorKind {
        ErrorKind::Database(err.to_string())
    }
}

/// Helper trait that makes it possible to supply contextual
/// information with an error.
pub trait ResultExt<T> {
    fn context<C: Display>(self, ctx: C) -> Result<T>;
}

impl <T, E: Into<Error>> ResultExt<T> for result::Result<T, E> {
    fn context<C: Display>(self, ctx: C) -> Result<T> {
        self.map_err(|err| Error {
            context: Some(format!("{}", ctx)),
            .. err.into()
        })
    }
}
