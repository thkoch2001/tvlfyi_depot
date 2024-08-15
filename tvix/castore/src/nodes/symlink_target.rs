use crate::ValidateNodeError;

/// A wrapper type for symlink targets.
/// Internally uses a [bytes::Bytes], but disallows empty targets and these
/// containing null bytes.
#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymlinkTarget(bytes::Bytes);

impl AsRef<[u8]> for SymlinkTarget {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl From<SymlinkTarget> for bytes::Bytes {
    fn from(value: SymlinkTarget) -> Self {
        value.0
    }
}

impl TryFrom<bytes::Bytes> for SymlinkTarget {
    type Error = ValidateNodeError;

    fn try_from(value: bytes::Bytes) -> Result<Self, Self::Error> {
        if value.is_empty() || value.contains(&b'\0') {
            return Err(ValidateNodeError::InvalidSymlinkTarget(value));
        }

        Ok(Self(value))
    }
}

impl TryFrom<&'static [u8]> for SymlinkTarget {
    type Error = ValidateNodeError;

    fn try_from(value: &'static [u8]) -> Result<Self, Self::Error> {
        if value.is_empty() || value.contains(&b'\0') {
            return Err(ValidateNodeError::InvalidSymlinkTarget(
                bytes::Bytes::from_static(value),
            ));
        }

        Ok(Self(bytes::Bytes::from_static(value)))
    }
}

impl TryFrom<&str> for SymlinkTarget {
    type Error = ValidateNodeError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(ValidateNodeError::InvalidSymlinkTarget(
                bytes::Bytes::copy_from_slice(value.as_bytes()),
            ));
        }

        Ok(Self(bytes::Bytes::copy_from_slice(value.as_bytes())))
    }
}
