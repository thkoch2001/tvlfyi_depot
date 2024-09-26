use std::alloc::Layout;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};

#[derive(Clone, Copy)]
#[repr(C)]
struct GSSmall {
    len: u32,
    data: [u8; 12],
}

#[derive(Clone, Copy)]
#[repr(transparent)]
struct StorageClassPtr(usize);

impl StorageClassPtr {
    fn transient(ptr: *const u8) -> Self {
        debug_assert!(
            (ptr as usize & 0b1) == 0,
            "pointer must be at least 2-byte aligned"
        );
        Self(ptr as usize)
    }

    fn persistent(ptr: *const u8) -> Self {
        debug_assert!(
            (ptr as usize & 0b1) == 0,
            "pointer must be at least 2-byte aligned"
        );
        Self((ptr as usize) | 0b1)
    }

    fn as_ptr(&self) -> *const u8 {
        (self.0 & !0b1) as *const u8
    }

    unsafe fn as_mut_ptr(&self) -> *mut u8 {
        (self.0 & !0b1) as *mut u8
    }

    fn is_transient(&self) -> bool {
        (self.0 & 0b1) == 0
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
struct GSLarge {
    len: u32,
    prefix: [u8; 4],
    data: StorageClassPtr,
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
    /// Creates a new transient German String from the given slice, copying the
    /// data in the process.
    pub fn transient(bytes: &[u8]) -> GermanString {
        if bytes.len() > u32::MAX as usize {
            panic!("GermanString maximum length is {} bytes", u32::MAX);
        }

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
                    StorageClassPtr::transient(ptr)
                },
            };

            large.prefix.copy_from_slice(&bytes[..4]);

            GermanString(GSRepr { large })
        }
    }

    /// Creates a new transient German String from the given owned bytes. Short
    /// strings will be copied into the string representation, long strings will
    /// be moved out of the given vector without additional allocations.
    pub fn transient_from_owned(bytes: Vec<u8>) -> GermanString {
        if bytes.len() > u32::MAX as usize {
            panic!("GermanString maximum length is {} bytes", u32::MAX);
        }

        if bytes.len() <= 12 {
            let mut s = GSSmall {
                len: bytes.len() as u32,
                data: [0u8; 12],
            };

            s.data[..bytes.len()].copy_from_slice(&bytes);
            GermanString(GSRepr { small: s })
        } else {
            let md = std::mem::ManuallyDrop::new(bytes);
            let mut large = GSLarge {
                len: md.len() as u32,
                prefix: [0u8; 4],
                data: StorageClassPtr::transient(md.as_ptr()),
            };

            large.prefix.copy_from_slice(&md[..4]);
            GermanString(GSRepr { large })
        }
    }

    /// Creates a persistent German String from a static data buffer.
    pub fn persistent(bytes: &'static [u8]) -> GermanString {
        if bytes.len() > u32::MAX as usize {
            panic!("GermanString maximum length is {} bytes", u32::MAX);
        }

        if bytes.len() <= 12 {
            let mut s = GSSmall {
                len: bytes.len() as u32,
                data: [0u8; 12],
            };

            s.data[..bytes.len()].copy_from_slice(&bytes);
            GermanString(GSRepr { small: s })
        } else {
            let mut large = GSLarge {
                len: bytes.len() as u32,
                prefix: [0u8; 4],
                data: StorageClassPtr::persistent(bytes.as_ptr()),
            };

            large.prefix.copy_from_slice(&bytes[..4]);
            GermanString(GSRepr { large })
        }
    }

    /// Creates a persistent German String by leaking the provided data.
    pub fn persistent_leak(bytes: Vec<u8>) -> GermanString {
        if bytes.len() > u32::MAX as usize {
            panic!("GermanString maximum length is {} bytes", u32::MAX);
        }

        if bytes.len() <= 12 {
            let mut s = GSSmall {
                len: bytes.len() as u32,
                data: [0u8; 12],
            };

            s.data[..bytes.len()].copy_from_slice(&bytes);
            GermanString(GSRepr { small: s })
        } else {
            let md = std::mem::ManuallyDrop::new(bytes);
            let mut large = GSLarge {
                len: md.len() as u32,
                prefix: [0u8; 4],
                data: StorageClassPtr::persistent(md.as_ptr()),
            };

            large.prefix.copy_from_slice(&md[..4]);
            GermanString(GSRepr { large })
        }
    }

    /// Creates a persistent German String from a static data buffer.
    pub fn persistent_from_str(s: &'static str) -> GermanString {
        GermanString::persistent(s.as_bytes())
    }

    pub fn len(&self) -> usize {
        // SAFETY: The length field is located in the same location for both
        // variants, reading it from either is safe.
        unsafe { self.0.small.len as usize }
    }

    pub fn as_bytes(&self) -> &[u8] {
        if self.len() > 12 {
            unsafe { std::slice::from_raw_parts(self.0.large.data.as_ptr(), self.len()) }
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
        unsafe {
            if self.len() > 12 && self.0.large.data.is_transient() {
                let layout = Layout::array::<u8>(self.len()).unwrap();
                std::alloc::dealloc(self.0.large.data.as_mut_ptr(), layout);
            }
        }
    }
}

impl PartialEq for GermanString {
    fn eq(&self, other: &GermanString) -> bool {
        if self.len() != other.len() {
            return false;
        }

        unsafe {
            if self.len() <= 12 {
                return self.0.small.data[..self.len()] == other.0.small.data[..other.len()];
            }
            return self.0.large.data.as_ptr() == other.0.large.data.as_ptr()
                || (self.0.large.prefix == other.0.large.prefix
                    && self.as_bytes() == other.as_bytes());
        }
    }
}

impl Eq for GermanString {}

impl Ord for GermanString {
    fn cmp(&self, other: &GermanString) -> Ordering {
        match (self.len().cmp(&12), other.len().cmp(&12)) {
            // two small strings
            (Ordering::Less | Ordering::Equal, Ordering::Less | Ordering::Equal) => unsafe {
                self.0.small.data[..self.len()].cmp(&other.0.small.data[..other.len()])
            },
            // two large strings
            (Ordering::Greater, Ordering::Greater) => unsafe {
                match self.0.large.prefix.cmp(&other.0.large.prefix) {
                    Ordering::Equal => self.as_bytes().cmp(other.as_bytes()),
                    ordering => ordering,
                }
            },

            // LHS large, RHS small
            (Ordering::Greater, _) => {
                let prefix_ordering =
                    unsafe { self.0.large.prefix.as_slice().cmp(&other.0.small.data[..4]) };

                if prefix_ordering != Ordering::Equal {
                    return prefix_ordering;
                }

                self.as_bytes().cmp(other.as_bytes())
            }

            // LHS small, RHS large
            (_, Ordering::Greater) => {
                let prefix_ordering =
                    unsafe { self.0.small.data[..4].cmp(other.0.large.prefix.as_slice()) };

                if prefix_ordering != Ordering::Equal {
                    return prefix_ordering;
                }

                self.as_bytes().cmp(other.as_bytes())
            }
        }
    }
}

impl PartialOrd for GermanString {
    fn partial_cmp(&self, other: &GermanString) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Debug for GermanString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        String::from_utf8_lossy(self.as_bytes()).fmt(f)
    }
}

impl Clone for GermanString {
    fn clone(&self) -> Self {
        unsafe {
            if self.len() <= 12 {
                return GermanString(GSRepr {
                    small: self.0.small.clone(),
                });
            }

            if self.0.large.data.is_transient() {
                return GermanString::transient(self.as_bytes());
            }

            return GermanString(GSRepr {
                large: self.0.large.clone(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    impl Arbitrary for GermanString {
        type Parameters = <String as Arbitrary>::Parameters;
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
            any_with::<String>(args)
                .prop_map(|s| GermanString::transient(s.as_bytes()))
                .boxed()
        }
    }

    #[test]
    fn test_empty_string() {
        let empty = GermanString::transient(b"");

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
        let short = GermanString::transient(b"meow");

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
        let long = GermanString::transient(input.as_bytes());

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

    proptest! {
        #[test]
        fn test_roundtrip_vec(input: Vec<u8>) {
            let gs = GermanString::transient_from_owned(input.clone());
            assert_eq!(input.len(), gs.len(), "length should match");

            let out = gs.as_bytes().to_owned();
            assert_eq!(input, out, "roundtrip should yield same bytes");
        }

        #[test]
        fn test_roundtrip_string(input: String) {
            let gs = GermanString::transient_from_owned(input.clone().into_bytes());
            assert_eq!(input.len(), gs.len(), "length should match");

            let out = String::from_utf8(gs.as_bytes().to_owned())
              .expect("string should be valid after roundtrip");

            assert_eq!(input, out, "roundtrip should yield same string");
        }

        // Test [`Eq`] implementation.
        #[test]
        fn test_eq(lhs: Vec<u8>, rhs: Vec<u8>) {
            let lhs_gs = GermanString::transient(lhs.as_slice());
            let rhs_gs = GermanString::transient(rhs.as_slice());

            assert_eq!(
                (lhs == rhs),
                (lhs_gs == rhs_gs),
                "Eq should match between std::String and GermanString ({:?} == {:?})",
                lhs, rhs,
            );
        }

        #[test]
        fn test_reflexivity(x: GermanString) {
            prop_assert!(x == x);
        }

        #[test]
        fn test_symmetry(x: GermanString, y: GermanString) {
            prop_assert_eq!(x == y, y == x);
        }

        #[test]
        fn test_transitivity(x: GermanString, y: GermanString, z: GermanString) {
            if x == y && y == z {
                assert!(x == z);
            }
        }
    }
}
