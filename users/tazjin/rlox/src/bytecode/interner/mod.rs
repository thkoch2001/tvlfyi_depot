//! String-interning implementation for values that are likely to
//! benefit from fast comparisons and deduplication (e.g. instances of
//! variable names).
//!
//! This uses a trick from the typed-arena crate for guaranteeing
//! stable addresses by never resizing the existing String buffer, and
//! collecting full buffers in a vector.

use std::collections::HashMap;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub struct InternedStr {
    id: usize,
}

#[derive(Default)]
pub struct Interner {
    map: HashMap<&'static str, InternedStr>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    pub fn with_capacity(cap: usize) -> Self {
        Interner {
            buf: String::with_capacity(cap),
            ..Default::default()
        }
    }

    pub fn intern<S: AsRef<str>>(&mut self, name: S) -> InternedStr {
        let name = name.as_ref();
        if let Some(&id) = self.map.get(name) {
            return id;
        }

        let name = self.alloc(name);
        let id = InternedStr {
            id: self.vec.len() as usize,
        };

        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    pub fn lookup<'a>(&'a self, id: InternedStr) -> &'a str {
        self.vec[id.id]
    }

    fn alloc<'a>(&'a mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned: &'a str = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        unsafe {
            // This is sound for two reasons:
            //
            // 1. This function (Interner::alloc) is private, which
            //    prevents users from allocating a supposedly static
            //    reference.
            //
            // 2. Interner::lookup explicitly shortens the lifetime of
            //    references that are handed out to that of the
            //    reference to self.
            return &*(interned as *const str);
        }
    }
}
