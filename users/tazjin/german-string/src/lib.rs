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

    fn len(&self) -> usize {
        // SAFETY: The length field is located in the same location for both
        // variants, reading it from either is safe.
        unsafe { self.0.small.len as usize }
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
