//! This module defines error types and conversions for issue that can
//! occur while dealing with persisted state machines.

use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error { SomeError }
