use rustc_hash::FxHashSet;
use serde::Serialize;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};

use super::NixString;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InternedStringId(usize);

#[derive(Default)]
pub struct Interner {
    map: HashMap<&'static str, InternedStringId>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    // pub fn with_capacity(cap: usize) -> Self {
    //     Interner {
    //         buf: String::with_capacity(cap),
    //         ..Default::default()
    //     }
    // }

    pub fn intern<S: AsRef<str>>(&mut self, name: S) -> InternedStringId {
        let name = name.as_ref();
        if let Some(&id) = self.map.get(name) {
            return id;
        }

        let name = self.alloc(name);
        let id = InternedStringId(self.vec.len());

        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    pub fn lookup<'a>(&'a self, id: InternedStringId) -> &'static str {
        self.vec[id.0]
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

thread_local! {
    static INTERNER: RefCell<Interner> = Default::default();
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternedString(InternedStringId);

impl InternedString {
    pub fn as_str(self) -> &'static str {
        INTERNER.with(|i| i.borrow().lookup(self.0))
    }

    pub fn to_string(self) -> String {
        self.as_str().to_owned()
    }
}

impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl From<InternedString> for NixString {
    fn from(value: InternedString) -> Self {
        NixString::from(value.as_str())
    }
}

impl<'a> From<&'a str> for InternedString {
    fn from(value: &'a str) -> Self {
        INTERNER.with(|i| InternedString(i.borrow_mut().intern(value)))
    }
}

impl<'a> From<&'a String> for InternedString {
    fn from(value: &'a String) -> Self {
        InternedString::from(value.as_str())
    }
}

impl From<String> for InternedString {
    fn from(value: String) -> Self {
        InternedString::from(value.as_str())
    }
}

impl From<NixString> for InternedString {
    fn from(value: NixString) -> Self {
        InternedString::from(std::str::from_utf8(&value).unwrap())
    }
}

impl<'a> From<Cow<'a, str>> for InternedString {
    fn from(value: Cow<'a, str>) -> Self {
        InternedString::from(value.as_ref())
    }
}

#[derive(Clone, Debug, Serialize, Hash, PartialEq, Eq)]
pub enum NixContextElement {
    /// A plain store path (e.g. source files copied to the store)
    Plain(InternedString),

    /// Single output of a derivation, represented by its name and its derivation path.
    Single {
        name: InternedString,
        derivation: InternedString,
    },

    /// A reference to a complete derivation
    /// including its source and its binary closure.
    /// It is used for the `drvPath` attribute context.
    /// The referred string is the store path to
    /// the derivation path.
    Derivation(InternedString),
}

/// Nix context strings representation in Tvix. This tracks a set of different kinds of string
/// dependencies that we can come across during manipulation of our language primitives, mostly
/// strings. There's some simple algebra of context strings and how they propagate w.r.t. primitive
/// operations, e.g. concatenation, interpolation and other string operations.
#[repr(transparent)]
#[derive(Clone, Debug, Serialize, Default)]
pub struct NixContext(FxHashSet<NixContextElement>);

impl From<NixContextElement> for NixContext {
    fn from(value: NixContextElement) -> Self {
        let mut set = FxHashSet::default();
        set.insert(value);
        Self(set)
    }
}

impl From<FxHashSet<NixContextElement>> for NixContext {
    fn from(value: FxHashSet<NixContextElement>) -> Self {
        Self(value)
    }
}

impl<const N: usize> From<[NixContextElement; N]> for NixContext {
    fn from(value: [NixContextElement; N]) -> Self {
        let mut set = FxHashSet::default();
        for elt in value {
            set.insert(elt);
        }
        Self(set)
    }
}

impl NixContext {
    /// Creates an empty context that can be populated
    /// and passed to form a contextful [NixString], albeit
    /// if the context is concretly empty, the resulting [NixString]
    /// will be contextless.
    pub fn new() -> Self {
        Self::default()
    }

    /// For internal consumers, we let people observe
    /// if the [NixContext] is actually empty or not
    /// to decide whether they want to skip the allocation
    /// of a full blown [HashSet].
    pub(crate) fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Consumes a new [NixContextElement] and add it if not already
    /// present in this context.
    pub fn append(mut self, other: NixContextElement) -> Self {
        self.0.insert(other);
        self
    }

    /// Extends the existing context with more context elements.
    pub fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = NixContextElement>,
    {
        self.0.extend(iter)
    }

    /// Copies from another [NixString] its context strings
    /// in this context.
    pub fn mimic(&mut self, other: &NixString) {
        if let Some(context) = other.context() {
            self.extend(context.iter().cloned());
        }
    }

    /// Iterates over "plain" context elements, e.g. sources imported
    /// in the store without more information, i.e. `toFile` or coerced imported paths.
    /// It yields paths to the store.
    pub fn iter_plain(&self) -> impl Iterator<Item = &str> {
        self.iter().filter_map(|elt| {
            if let NixContextElement::Plain(s) = elt {
                Some(s.as_str())
            } else {
                None
            }
        })
    }

    /// Iterates over "full derivations" context elements, e.g. something
    /// referring to their `drvPath`, i.e. their full sources and binary closure.
    /// It yields derivation paths.
    pub fn iter_derivation(&self) -> impl Iterator<Item = &str> {
        self.iter().filter_map(|elt| {
            if let NixContextElement::Derivation(s) = elt {
                Some(s.as_str())
            } else {
                None
            }
        })
    }

    /// Iterates over "single" context elements, e.g. single derived paths,
    /// or also known as the single output of a given derivation.
    /// The first element of the tuple is the output name
    /// and the second element is the derivation path.
    pub fn iter_single_outputs(&self) -> impl Iterator<Item = (&str, &str)> {
        self.iter().filter_map(|elt| {
            if let NixContextElement::Single { name, derivation } = elt {
                Some((name.as_str(), derivation.as_str()))
            } else {
                None
            }
        })
    }

    /// Iterates over any element of the context.
    pub fn iter(&self) -> impl Iterator<Item = &NixContextElement> {
        self.0.iter()
    }

    /// Produces a list of owned references to this current context,
    /// no matter its type.
    pub fn to_owned_references(self) -> Vec<InternedString> {
        self.0
            .into_iter()
            .map(|ctx| match ctx {
                NixContextElement::Derivation(drv_path) => drv_path,
                NixContextElement::Plain(store_path) => store_path,
                NixContextElement::Single { derivation, .. } => derivation,
            })
            .collect()
    }
}

impl IntoIterator for NixContext {
    type Item = NixContextElement;

    type IntoIter = std::collections::hash_set::IntoIter<NixContextElement>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
