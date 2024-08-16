use std::alloc::Layout;

#[derive(Clone, Copy)]
#[repr(C)]
struct GSSmall {
    len: u32,
    data: [u8; 12],
}

#[derive(Clone, Copy)]
#[repr(C)]
struct GSLarge {
    len: u32,
    prefix: [u8; 4],
    data: *mut u8,
}

const _ASSERT_VARIANTS_SIZE: () = assert!(
    std::mem::size_of::<GSSmall>() == std::mem::size_of::<GSLarge>(),
    "German String variants must have the same size"
);

union GSRepr {
    small: GSSmall,
    large: GSLarge,
}

#[repr(transparent)]
pub struct GermanString(GSRepr);

const _ASSERT_GSTRING_SIZE: () = assert!(
    std::mem::size_of::<GermanString>() == 16,
    "German String should be 16 bytes in size",
);

impl GermanString {
    // Creates a new transient German String from the given bytes. Transient
    // strings are destroyed when the object is destroyed. Persistent strings
    // are not supported yet.
    pub fn new_transient(bytes: &[u8]) -> GermanString {
        if bytes.len() <= 12 {
            let mut s = GSSmall {
                len: bytes.len() as u32,
                data: [0u8; 12],
            };
            s.data[..bytes.len()].copy_from_slice(bytes);
            GermanString(GSRepr { small: s })
        } else {
            let layout = Layout::array::<u8>(bytes.len()).unwrap();
            let mut large = GSLarge {
                len: bytes.len() as u32,
                prefix: [0u8; 4],
                data: unsafe {
                    let ptr = std::alloc::alloc(layout);
                    if ptr.is_null() {
                        std::alloc::handle_alloc_error(layout);
                    }
                    std::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len());
                    ptr
                },
            };

            large.prefix.copy_from_slice(&bytes[..4]);

            GermanString(GSRepr { large })
        }
    }

    pub fn len(&self) -> usize {
        // SAFETY: The length field is located in the same location for both
        // variants, reading it from either is safe.
        unsafe { self.0.small.len as usize }
    }

    pub fn as_bytes(&self) -> &[u8] {
        if self.len() > 12 {
            unsafe { std::slice::from_raw_parts(self.0.large.data, self.len()) }
        } else {
            unsafe { &self.0.small.data.as_ref()[..self.len()] }
        }
    }

    pub fn as_str(&self) -> Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(self.as_bytes())
    }
}

impl Drop for GermanString {
    fn drop(&mut self) {
        if self.len() > 12 {
            let layout = Layout::array::<u8>(self.len()).unwrap();
            unsafe {
                std::alloc::dealloc(self.0.large.data, layout);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_string() {
        let empty = GermanString::new_transient(b"");

        assert_eq!(empty.len(), 0, "empty string should be empty");
        assert_eq!(empty.as_bytes(), b"", "empty string should contain nothing");
        assert_eq!(
            empty.as_str().expect("empty string is valid UTF-8"),
            "",
            "empty string should contain empty string"
        );
    }

    #[test]
    fn test_short_string() {
        let short = GermanString::new_transient(b"meow");

        assert_eq!(short.len(), 4, "'meow' is four characters");
        assert_eq!(
            short.as_bytes(),
            b"meow",
            "short string returns correct bytes"
        );
        assert_eq!(
            short.as_str().expect("'meow' is valid UTF-8"),
            "meow",
            "short string returns correct string"
        );
    }

    #[test]
    fn test_long_string() {
        let input: &str = "This code was written at https://signal.live";
        let long = GermanString::new_transient(input.as_bytes());

        assert_eq!(long.len(), 44, "long string has correct length");
        assert_eq!(
            long.as_bytes(),
            input.as_bytes(),
            "long string returns correct bytes"
        );

        assert_eq!(
            long.as_str().expect("input is valid UTF-8"),
            input,
            "long string returns correct string"
        );
    }
}
