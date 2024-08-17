use bstr::ByteSlice;
use std::fmt::{self, Debug, Display};

/// A wrapper type for validated path components in the castore model.
/// Internally uses a [bytes::Bytes], but disallows
/// slashes, and null bytes to be present, as well as
/// '.', '..' and the empty string.
#[repr(transparent)]
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathComponent {
    pub(super) inner: bytes::Bytes,
}

impl AsRef<[u8]> for PathComponent {
    fn as_ref(&self) -> &[u8] {
        self.inner.as_ref()
    }
}

impl From<PathComponent> for bytes::Bytes {
    fn from(value: PathComponent) -> Self {
        value.inner
    }
}

pub(super) fn validate_name<B: AsRef<[u8]>>(name: B) -> Result<B, PathComponentError> {
    let v = name.as_ref();

    if v.is_empty() {
        return Err(PathComponentError::Empty);
    }
    if v == *b".." {
        return Err(PathComponentError::Parent);
    }
    if v == *b"." {
        return Err(PathComponentError::CurDir);
    }
    if v.contains(&0x00) {
        return Err(PathComponentError::Null);
    }
    if v.contains(&b'/') {
        return Err(PathComponentError::Slashes);
    }

    Ok(name)
}

impl TryFrom<bytes::Bytes> for PathComponent {
    type Error = PathComponentError;

    fn try_from(value: bytes::Bytes) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: validate_name(value)?,
        })
    }
}

impl TryFrom<&'static [u8]> for PathComponent {
    type Error = PathComponentError;

    fn try_from(value: &'static [u8]) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: bytes::Bytes::from_static(validate_name(value)?),
        })
    }
}

impl TryFrom<&str> for PathComponent {
    type Error = PathComponentError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: bytes::Bytes::copy_from_slice(validate_name(value.as_bytes())?),
        })
    }
}

impl TryFrom<&std::ffi::CStr> for PathComponent {
    type Error = PathComponentError;

    fn try_from(value: &std::ffi::CStr) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: bytes::Bytes::copy_from_slice(validate_name(value.to_bytes())?),
        })
    }
}

impl Debug for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(self.inner.as_bstr(), f)
    }
}

impl Display for PathComponent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self.inner.as_bstr(), f)
    }
}

/// Errors created when parsing / validating [PathComponent].
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum PathComponentError {
    #[error("cannot be empty")]
    Empty,
    #[error("cannot contain null bytes")]
    Null,
    #[error("cannot be '.'")]
    CurDir,
    #[error("cannot be '..'")]
    Parent,
    #[error("cannot contain slashes")]
    Slashes,
    // FUTUREWORK: reasonable maximum length?
}
