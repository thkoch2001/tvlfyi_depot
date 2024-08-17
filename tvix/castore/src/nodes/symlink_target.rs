use bstr::ByteSlice;
use std::fmt::{self, Debug, Display};

/// A wrapper type for symlink targets.
/// Internally uses a [bytes::Bytes], but disallows empty targets and those
/// containing null bytes.
#[repr(transparent)]
#[derive(Clone, PartialEq, Eq)]
pub struct SymlinkTarget {
    inner: bytes::Bytes,
}

/// The maximum length a symlink target can have.
/// Linux allows 4095 bytes here.
pub const MAX_TARGET_LEN: usize = 4095;

impl AsRef<[u8]> for SymlinkTarget {
    fn as_ref(&self) -> &[u8] {
        self.inner.as_ref()
    }
}

impl From<SymlinkTarget> for bytes::Bytes {
    fn from(value: SymlinkTarget) -> Self {
        value.inner
    }
}

fn validate_symlink_target<B: AsRef<[u8]>>(symlink_target: B) -> Result<B, SymlinkTargetError> {
    let v = symlink_target.as_ref();

    if v.is_empty() {
        return Err(SymlinkTargetError::Empty);
    }
    if v.len() > MAX_TARGET_LEN {
        return Err(SymlinkTargetError::TooLong);
    }
    if v.contains(&0x00) {
        return Err(SymlinkTargetError::Null);
    }

    Ok(symlink_target)
}

impl TryFrom<bytes::Bytes> for SymlinkTarget {
    type Error = SymlinkTargetError;

    fn try_from(value: bytes::Bytes) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: validate_symlink_target(value)?,
        })
    }
}

impl TryFrom<&'static [u8]> for SymlinkTarget {
    type Error = SymlinkTargetError;

    fn try_from(value: &'static [u8]) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: bytes::Bytes::from_static(validate_symlink_target(value)?),
        })
    }
}

impl TryFrom<&str> for SymlinkTarget {
    type Error = SymlinkTargetError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self {
            inner: bytes::Bytes::copy_from_slice(validate_symlink_target(value.as_bytes())?),
        })
    }
}

impl Debug for SymlinkTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(self.inner.as_bstr(), f)
    }
}

impl Display for SymlinkTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self.inner.as_bstr(), f)
    }
}

/// Errors created when constructing / converting to [SymlinkTarget].
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum SymlinkTargetError {
    #[error("cannot be empty")]
    Empty,
    #[error("cannot contain null bytes")]
    Null,
    #[error("cannot be over {} bytes long", MAX_TARGET_LEN)]
    TooLong,
}
