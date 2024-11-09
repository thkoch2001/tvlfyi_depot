//! This module implements Nix language strings.
//!
//! See [`NixString`] for more information about the internals of string values

use bstr::{BStr, BString, ByteSlice, Chars};
use nohash_hasher::BuildNoHashHasher;
use rnix::ast;
#[cfg(feature = "no_leak")]
use rustc_hash::FxHashSet;
use rustc_hash::FxHasher;
use std::alloc::dealloc;
use std::alloc::{alloc, handle_alloc_error, Layout};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::ffi::c_void;
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr::{self, NonNull};
use std::slice;

use serde::de::{Deserializer, Visitor};
use serde::Deserialize;

mod context;

pub use context::{NixContext, NixContextElement};

/// This type is never instantiated, but serves to document the memory layout of the actual heap
/// allocation for Nix strings.
#[allow(dead_code)]
struct NixStringInner {
    /// The string context, if any.  Note that this is boxed to take advantage of the null pointer
    /// niche, otherwise this field ends up being very large:
    ///
    /// ```notrust
    /// >> std::mem::size_of::<Option<HashSet<String>>>()
    /// 48
    ///
    /// >> std::mem::size_of::<Option<Box<HashSet<String>>>>()
    /// 8
    /// ```
    context: Option<Box<NixContext>>,
    /// The length of the data, stored *inline in the allocation*
    length: usize,
    /// The actual data for the string itself. Will always be `length` bytes long
    data: [u8],
}

#[allow(clippy::zst_offset)]
impl NixStringInner {
    /// Construct a [`Layout`] for a nix string allocation with the given length.
    ///
    /// Returns a tuple of:
    /// 1. The layout itself.
    /// 2. The offset of [`Self::length`] within the allocation, assuming the allocation starts at 0
    /// 3. The offset of the data array within the allocation, assuming the allocation starts at 0
    fn layout(len: usize) -> (Layout, usize, usize) {
        let layout = Layout::new::<Option<Box<NixContext>>>();
        let (layout, len_offset) = layout.extend(Layout::new::<usize>()).unwrap();
        let (layout, data_offset) = layout.extend(Layout::array::<u8>(len).unwrap()).unwrap();
        (layout, len_offset, data_offset)
    }

    /// Returns the [`Layout`] for an *already-allocated* nix string, loading the length from the
    /// pointer.
    ///
    /// Returns a tuple of:
    /// 1. The layout itself.
    /// 2. The offset of [`Self::length`] within the allocation, assuming the allocation starts at 0
    /// 3. The offset of the data array within the allocation, assuming the allocation starts at 0
    ///
    /// # Safety
    ///
    /// This function must only be called on a pointer that has been properly initialized with
    /// [`Self::alloc`]. The data buffer may not necessarily be initialized
    unsafe fn layout_of(this: NonNull<c_void>) -> (Layout, usize, usize) {
        let layout = Layout::new::<Option<Box<NixContext>>>();
        let (_, len_offset) = layout.extend(Layout::new::<usize>()).unwrap();
        // SAFETY: Layouts are linear, so even though we haven't involved data at all yet, we know
        // the len_offset is a valid offset into the second field of the allocation
        let len = *(this.as_ptr().add(len_offset) as *const usize);
        Self::layout(len)
    }

    /// Allocate an *uninitialized* nix string with the given length. Writes the length to the
    /// length value in the pointer, but leaves both context and data uninitialized
    ///
    /// This function is safe to call (as constructing pointers of any sort of validity is always
    /// safe in Rust) but it is unsafe to use the resulting pointer to do anything other than
    ///
    /// 1. Read the length
    /// 2. Write the context
    /// 3. Write the data
    ///
    /// until the string is fully initialized
    fn alloc(len: usize) -> NonNull<c_void> {
        let (layout, len_offset, _data_offset) = Self::layout(len);
        debug_assert_ne!(layout.size(), 0);
        unsafe {
            // SAFETY: Layout has non-zero size, since the layout of the context and the
            // layout of the len both have non-zero size
            let ptr = alloc(layout);

            if let Some(this) = NonNull::new(ptr as *mut _) {
                // SAFETY: We've allocated with a layout that causes the len_offset to be in-bounds
                // and writeable, and if the allocation succeeded it won't wrap
                ((this.as_ptr() as *mut u8).add(len_offset) as *mut usize).write(len);
                debug_assert_eq!(Self::len(this), len);
                this
            } else {
                handle_alloc_error(layout);
            }
        }
    }

    /// Deallocate the Nix string at the given pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`]
    unsafe fn dealloc(this: NonNull<c_void>) {
        let (layout, _, _) = Self::layout_of(this);
        // SAFETY: okay because of the safety guarantees of this method
        dealloc(this.as_ptr() as *mut u8, layout)
    }

    /// Return the length of the Nix string at the given pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`]
    unsafe fn len(this: NonNull<c_void>) -> usize {
        let (_, len_offset, _) = Self::layout_of(this);
        // SAFETY: As long as the safety guarantees of this method are upheld, we've allocated with
        // a layout that causes the len_offset to be in-bounds and writeable, and if the allocation
        // succeeded it won't wrap
        *(this.as_ptr().add(len_offset) as *const usize)
    }

    /// Return a pointer to the context value within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`]
    unsafe fn context_ptr(this: NonNull<c_void>) -> *mut Option<Box<NixContext>> {
        // SAFETY: The context is the first field in the layout of the allocation
        this.as_ptr() as *mut Option<Box<NixContext>>
    }

    /// Construct a shared reference to the context value within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`], and where the context has been properly initialized (by writing to the
    /// pointer returned from [`Self::context_ptr`]).
    ///
    /// Also, all the normal Rust rules about pointer-to-reference conversion apply. See
    /// [`NonNull::as_ref`] for more.
    unsafe fn context_ref<'a>(this: NonNull<c_void>) -> &'a Option<Box<NixContext>> {
        Self::context_ptr(this).as_ref().unwrap()
    }

    /// Construct a mutable reference to the context value within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`], and where the context has been properly initialized (by writing to the
    /// pointer returned from [`Self::context_ptr`]).
    ///
    /// Also, all the normal Rust rules about pointer-to-reference conversion apply. See
    /// [`NonNull::as_mut`] for more.
    unsafe fn context_mut<'a>(this: NonNull<c_void>) -> &'a mut Option<Box<NixContext>> {
        Self::context_ptr(this).as_mut().unwrap()
    }

    /// Return a pointer to the data array within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`]
    unsafe fn data_ptr(this: NonNull<c_void>) -> *mut u8 {
        let (_, _, data_offset) = Self::layout_of(this);
        // SAFETY: data is the third field in the layout of the allocation
        this.as_ptr().add(data_offset) as *mut u8
    }

    /// Construct a shared reference to the data slice within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`], and where the data array has been properly initialized (by writing to the
    /// pointer returned from [`Self::data_ptr`]).
    ///
    /// Also, all the normal Rust rules about pointer-to-reference conversion apply. See
    /// [`slice::from_raw_parts`] for more.
    unsafe fn data_slice<'a>(this: NonNull<c_void>) -> &'a [u8] {
        let len = Self::len(this);
        let data = Self::data_ptr(this);
        slice::from_raw_parts(data, len)
    }

    /// Construct a mutable reference to the data slice within the given Nix string pointer
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`], and where the data array has been properly initialized (by writing to the
    /// pointer returned from [`Self::data_ptr`]).
    ///
    /// Also, all the normal Rust rules about pointer-to-reference conversion apply. See
    /// [`slice::from_raw_parts_mut`] for more.
    #[allow(dead_code)]
    unsafe fn data_slice_mut<'a>(this: NonNull<c_void>) -> &'a mut [u8] {
        let len = Self::len(this);
        let data = Self::data_ptr(this);
        slice::from_raw_parts_mut(data, len)
    }

    /// Clone the Nix string pointed to by this pointer, and return a pointer to a new Nix string
    /// containing the same data and context.
    ///
    /// # Safety
    ///
    /// This function must only be called with a pointer that has been properly initialized with
    /// [`Self::alloc`], and where the context has been properly initialized (by writing to the
    /// pointer returned from [`Self::context_ptr`]), and the data array has been properly
    /// initialized (by writing to the pointer returned from [`Self::data_ptr`]).
    unsafe fn clone(this: NonNull<c_void>) -> NonNull<c_void> {
        let (layout, _, _) = Self::layout_of(this);
        let ptr = alloc(layout);
        if let Some(new) = NonNull::new(ptr as *mut _) {
            ptr::copy_nonoverlapping(this.as_ptr(), new.as_ptr(), layout.size());
            Self::context_ptr(new).write(Self::context_ref(this).clone());
            new
        } else {
            handle_alloc_error(layout);
        }
    }
}

#[derive(Default)]
struct InternerInner {
    #[allow(clippy::disallowed_types)] // Not using the default hasher
    map: std::collections::HashMap<u64, NonNull<c_void>, BuildNoHashHasher<u64>>,
    #[cfg(feature = "no_leak")]
    #[allow(clippy::disallowed_types)] // Not using the default hasher
    interned_strings: FxHashSet<NonNull<c_void>>,
}

unsafe impl Send for InternerInner {}

fn hash<T>(s: T) -> u64
where
    T: Hash,
{
    let mut hasher = FxHasher::default();
    s.hash(&mut hasher);
    hasher.finish()
}

impl InternerInner {
    pub fn intern(&mut self, s: &[u8]) -> NixString {
        let hash = hash(s);
        if let Some(s) = self.map.get(&hash) {
            return NixString(*s);
        }

        let string = NixString::new_inner(s, None);
        self.map.insert(hash, string.0);
        #[cfg(feature = "no_leak")]
        self.interned_strings.insert(string.0);
        string
    }
}

#[derive(Default)]
struct Interner(RefCell<InternerInner>);

impl Interner {
    pub fn intern(&self, s: &[u8]) -> NixString {
        self.0.borrow_mut().intern(s)
    }

    #[cfg(feature = "no_leak")]
    pub fn is_interned_string(&self, string: &NixString) -> bool {
        self.0.borrow().interned_strings.contains(&string.0)
    }
}

thread_local! {
    static INTERNER: Interner = Interner::default();
}

/// Nix string values
///
/// # Internals
///
/// For performance reasons (to keep allocations small, and to avoid indirections), [`NixString`] is
/// represented as a single *thin* pointer to a packed data structure containing the
/// [context][NixContext] and the string data itself (which is a raw byte array, to match the Nix
/// string semantics that allow any array of bytes to be represented by a string).

/// This memory representation is documented in [`NixStringInner`], but since Rust prefers to deal
/// with slices via *fat pointers* (pointers that include the length in the *pointer*, not in the
/// heap allocation), we have to do mostly manual layout management and allocation for this
/// representation. See the documentation for the methods of [`NixStringInner`] for more information
pub struct NixString(NonNull<c_void>);

unsafe impl Send for NixString {}
unsafe impl Sync for NixString {}

impl Drop for NixString {
    #[cfg(not(feature = "no_leak"))]
    fn drop(&mut self) {
        if self.context().is_some() {
            // SAFETY: There's no way to construct a NixString that doesn't leave the allocation correct
            // according to the rules of dealloc
            unsafe {
                NixStringInner::dealloc(self.0);
            }
        }
    }

    #[cfg(feature = "no_leak")]
    fn drop(&mut self) {
        if INTERNER.with(|i| i.is_interned_string(self)) {
            return;
        }

        // SAFETY: There's no way to construct a NixString that doesn't leave the allocation correct
        // according to the rules of dealloc
        unsafe {
            NixStringInner::dealloc(self.0);
        }
    }
}

impl Clone for NixString {
    fn clone(&self) -> Self {
        if cfg!(feature = "no_leak") || self.context().is_some() {
            // SAFETY: There's no way to construct a NixString that doesn't leave the allocation correct
            // according to the rules of clone
            unsafe { Self(NixStringInner::clone(self.0)) }
        } else {
            // SAFETY:
            //
            // - NixStrings are never mutated
            // - NixStrings are never freed
            Self(self.0)
        }
    }
}

impl Debug for NixString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ctx) = self.context() {
            f.debug_struct("NixString")
                .field("context", ctx)
                .field("data", &self.as_bstr())
                .finish()
        } else {
            write!(f, "{:?}", self.as_bstr())
        }
    }
}

impl PartialEq for NixString {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || self.as_bstr() == other.as_bstr()
    }
}

impl Eq for NixString {}

impl PartialEq<&[u8]> for NixString {
    fn eq(&self, other: &&[u8]) -> bool {
        **self == **other
    }
}

impl PartialEq<&str> for NixString {
    fn eq(&self, other: &&str) -> bool {
        **self == other.as_bytes()
    }
}

impl PartialOrd for NixString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NixString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.0 == other.0 {
            return std::cmp::Ordering::Equal;
        }
        self.as_bstr().cmp(other.as_bstr())
    }
}

impl From<Box<BStr>> for NixString {
    fn from(value: Box<BStr>) -> Self {
        Self::new(&value, None)
    }
}

impl From<BString> for NixString {
    fn from(value: BString) -> Self {
        Self::new(&value, None)
    }
}

impl From<&BStr> for NixString {
    fn from(value: &BStr) -> Self {
        value.to_owned().into()
    }
}

impl From<&[u8]> for NixString {
    fn from(value: &[u8]) -> Self {
        Self::from(value.to_owned())
    }
}

impl From<Vec<u8>> for NixString {
    fn from(value: Vec<u8>) -> Self {
        value.into_boxed_slice().into()
    }
}

impl From<Box<[u8]>> for NixString {
    fn from(value: Box<[u8]>) -> Self {
        Self::new(&value, None)
    }
}

impl From<&str> for NixString {
    fn from(s: &str) -> Self {
        s.as_bytes().into()
    }
}

impl From<String> for NixString {
    fn from(s: String) -> Self {
        s.into_bytes().into()
    }
}

impl From<Box<str>> for NixString {
    fn from(s: Box<str>) -> Self {
        s.into_boxed_bytes().into()
    }
}

impl From<ast::Ident> for NixString {
    fn from(ident: ast::Ident) -> Self {
        ident.ident_token().unwrap().text().into()
    }
}

impl<'a> From<&'a NixString> for &'a BStr {
    fn from(s: &'a NixString) -> Self {
        s.as_bstr()
    }
}

// No impl From<NixString> for String, that one quotes.

impl From<NixString> for BString {
    fn from(s: NixString) -> Self {
        s.as_bstr().to_owned()
    }
}

impl AsRef<[u8]> for NixString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Borrow<BStr> for NixString {
    fn borrow(&self) -> &BStr {
        self.as_bstr()
    }
}

impl Hash for NixString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_bstr().hash(state)
    }
}

impl<'de> Deserialize<'de> for NixString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StringVisitor;

        impl<'de> Visitor<'de> for StringVisitor {
            type Value = NixString;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Nix string")
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.into())
            }
        }

        deserializer.deserialize_string(StringVisitor)
    }
}

impl Deref for NixString {
    type Target = BStr;

    fn deref(&self) -> &Self::Target {
        self.as_bstr()
    }
}

#[cfg(feature = "arbitrary")]
mod arbitrary {
    use super::*;
    use proptest::prelude::{any_with, Arbitrary};
    use proptest::strategy::{BoxedStrategy, Strategy};

    impl Arbitrary for NixString {
        type Parameters = <String as Arbitrary>::Parameters;

        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
            any_with::<String>(args).prop_map(Self::from).boxed()
        }
    }
}

/// Set non-scientifically. TODO(aspen): think more about what this should be
const INTERN_THRESHOLD: usize = 32;

impl NixString {
    fn new(contents: &[u8], context: Option<Box<NixContext>>) -> Self {
        debug_assert!(
            !context.as_deref().is_some_and(NixContext::is_empty),
            "BUG: initialized with empty context"
        );

        if !cfg!(feature = "no_leak") /* It's only safe to intern if we leak strings, since there's
                                       * nothing yet preventing interned strings from getting freed
                                       * (and then used by other copies) otherwise
                                       */
            && contents.len() <= INTERN_THRESHOLD
            && context.is_none()
        {
            return INTERNER.with(|i| i.intern(contents));
        }

        Self::new_inner(contents, context)
    }

    fn new_inner(contents: &[u8], context: Option<Box<NixContext>>) -> Self {
        // SAFETY: We're always fully initializing a NixString here:
        //
        // 1. NixStringInner::alloc sets up the len for us
        // 2. We set the context, using ptr::write to make sure that the uninitialized memory isn't
        //    read or dropped
        // 3. We set the data, using copy_from_nonoverlapping to make sure that the uninitialized
        //    memory isn't read or dropped
        //
        // Only *then* can we construct a NixString
        unsafe {
            let inner = NixStringInner::alloc(contents.len());
            NixStringInner::context_ptr(inner).write(context);
            NixStringInner::data_ptr(inner)
                .copy_from_nonoverlapping(contents.as_ptr(), contents.len());
            Self(inner)
        }
    }

    pub fn new_inherit_context_from<T>(other: &NixString, new_contents: T) -> Self
    where
        NixString: From<T>,
    {
        Self::new(
            Self::from(new_contents).as_ref(),
            other.context().map(|c| Box::new(c.clone())),
        )
    }

    pub fn new_context_from<T>(context: NixContext, contents: T) -> Self
    where
        NixString: From<T>,
    {
        Self::new(
            Self::from(contents).as_ref(),
            if context.is_empty() {
                None
            } else {
                Some(Box::new(context))
            },
        )
    }

    pub fn as_bstr(&self) -> &BStr {
        BStr::new(self.as_bytes())
    }

    pub fn as_bytes(&self) -> &[u8] {
        // SAFETY: There's no way to construct an uninitialized NixString (see the SAFETY comment in
        // `new`)
        unsafe { NixStringInner::data_slice(self.0) }
    }

    pub fn into_bstring(self) -> BString {
        self.as_bstr().to_owned()
    }

    /// Return a displayable representation of the string as an
    /// identifier.
    ///
    /// This is used when printing out strings used as e.g. attribute
    /// set keys, as those are only escaped in the presence of special
    /// characters.
    pub fn ident_str(&self) -> Cow<str> {
        let escaped = match self.to_str_lossy() {
            Cow::Borrowed(s) => nix_escape_string(s),
            Cow::Owned(s) => nix_escape_string(&s).into_owned().into(),
        };
        match escaped {
            // A borrowed string is unchanged and can be returned as
            // is.
            Cow::Borrowed(_) => {
                if is_valid_nix_identifier(&escaped) && !is_keyword(&escaped) {
                    escaped
                } else {
                    Cow::Owned(format!("\"{}\"", escaped))
                }
            }

            // An owned string has escapes, and needs the outer quotes
            // for display.
            Cow::Owned(s) => Cow::Owned(format!("\"{}\"", s)),
        }
    }

    pub fn concat(&self, other: &Self) -> Self {
        let mut s = self.to_vec();
        s.extend(&(***other));

        let context = [self.context(), other.context()]
            .into_iter()
            .flatten()
            .fold(NixContext::new(), |mut acc_ctx, new_ctx| {
                // TODO: consume new_ctx?
                acc_ctx.extend(new_ctx.iter().cloned());
                acc_ctx
            });
        Self::new_context_from(context, s)
    }

    pub(crate) fn context(&self) -> Option<&NixContext> {
        // SAFETY: There's no way to construct an uninitialized or invalid NixString (see the SAFETY
        // comment in `new`).
        //
        // Also, we're using the same lifetime and mutability as self, to fit the
        // pointer-to-reference conversion rules
        let context = unsafe { NixStringInner::context_ref(self.0).as_deref() };

        debug_assert!(
            !context.is_some_and(NixContext::is_empty),
            "BUG: empty context"
        );

        context
    }

    pub(crate) fn context_mut(&mut self) -> &mut Option<Box<NixContext>> {
        // SAFETY: There's no way to construct an uninitialized or invalid NixString (see the SAFETY
        // comment in `new`).
        //
        // Also, we're using the same lifetime and mutability as self, to fit the
        // pointer-to-reference conversion rules
        let context = unsafe { NixStringInner::context_mut(self.0) };

        debug_assert!(
            !context.as_deref().is_some_and(NixContext::is_empty),
            "BUG: empty context"
        );

        context
    }

    /// Iterates over all context elements.
    /// See [iter_plain], [iter_derivation], [iter_single_outputs].
    pub fn iter_context(&self) -> impl Iterator<Item = &NixContext> {
        self.context().into_iter()
    }

    /// Iterates over "plain" context elements, e.g. sources imported
    /// in the store without more information, i.e. `toFile` or coerced imported paths.
    /// It yields paths to the store.
    pub fn iter_ctx_plain(&self) -> impl Iterator<Item = &str> {
        self.iter_context().flat_map(|context| context.iter_plain())
    }

    /// Iterates over "full derivations" context elements, e.g. something
    /// referring to their `drvPath`, i.e. their full sources and binary closure.
    /// It yields derivation paths.
    pub fn iter_ctx_derivation(&self) -> impl Iterator<Item = &str> {
        return self
            .iter_context()
            .flat_map(|context| context.iter_derivation());
    }

    /// Iterates over "single" context elements, e.g. single derived paths,
    /// or also known as the single output of a given derivation.
    /// The first element of the tuple is the output name
    /// and the second element is the derivation path.
    pub fn iter_ctx_single_outputs(&self) -> impl Iterator<Item = (&str, &str)> {
        return self
            .iter_context()
            .flat_map(|context| context.iter_single_outputs());
    }

    /// Returns whether this Nix string possess a context or not.
    pub fn has_context(&self) -> bool {
        self.context().is_some()
    }

    /// This clears the context of the string, returning
    /// the removed dependency tracking information.
    pub fn take_context(&mut self) -> Option<Box<NixContext>> {
        self.context_mut().take()
    }

    /// This clears the context of that string, losing
    /// all dependency tracking information.
    pub fn clear_context(&mut self) {
        let _ = self.take_context();
    }

    pub fn chars(&self) -> Chars<'_> {
        self.as_bstr().chars()
    }
}

fn nix_escape_char(ch: char, next: Option<&char>) -> Option<&'static str> {
    match (ch, next) {
        ('\\', _) => Some("\\\\"),
        ('"', _) => Some("\\\""),
        ('\n', _) => Some("\\n"),
        ('\t', _) => Some("\\t"),
        ('\r', _) => Some("\\r"),
        ('$', Some('{')) => Some("\\$"),
        _ => None,
    }
}

/// Return true if this string is a keyword -- character strings
/// which lexically match the "identifier" production but are not
/// parsed as identifiers.  See also cppnix commit
/// b72bc4a972fe568744d98b89d63adcd504cb586c.
fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "then" | "else" | "assert" | "with" | "let" | "in" | "rec" | "inherit"
    )
}

/// Return true if this string can be used as an identifier in Nix.
fn is_valid_nix_identifier(s: &str) -> bool {
    // adapted from rnix-parser's tokenizer.rs
    let mut chars = s.chars();
    match chars.next() {
        Some('a'..='z' | 'A'..='Z' | '_') => (),
        _ => return false,
    }
    for c in chars {
        match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' | '\'' => (),
            _ => return false,
        }
    }
    true
}

/// Escape a Nix string for display, as most user-visible representation
/// are escaped strings.
///
/// Note that this does not add the outer pair of surrounding quotes.
fn nix_escape_string(input: &str) -> Cow<str> {
    let mut iter = input.char_indices().peekable();

    while let Some((i, c)) = iter.next() {
        if let Some(esc) = nix_escape_char(c, iter.peek().map(|(_, c)| c)) {
            let mut escaped = String::with_capacity(input.len());
            escaped.push_str(&input[..i]);
            escaped.push_str(esc);

            // In theory we calculate how many bytes it takes to represent `esc`
            // in UTF-8 and use that for the offset. It is, however, safe to
            // assume that to be 1, as all characters that can be escaped in a
            // Nix string are ASCII.
            let mut inner_iter = input[i + 1..].chars().peekable();
            while let Some(c) = inner_iter.next() {
                match nix_escape_char(c, inner_iter.peek()) {
                    Some(esc) => escaped.push_str(esc),
                    None => escaped.push(c),
                }
            }

            return Cow::Owned(escaped);
        }
    }

    Cow::Borrowed(input)
}

impl Display for NixString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("\"")?;
        f.write_str(&nix_escape_string(&self.to_str_lossy()))?;
        f.write_str("\"")
    }
}

#[cfg(all(test, feature = "arbitrary"))]
mod tests {
    use test_strategy::proptest;

    use super::*;

    use crate::properties::{eq_laws, hash_laws, ord_laws};

    #[test]
    fn size() {
        assert_eq!(std::mem::size_of::<NixString>(), 8);
    }

    #[proptest]
    fn clone_strings(s: NixString) {
        drop(s.clone())
    }

    eq_laws!(NixString);
    hash_laws!(NixString);
    ord_laws!(NixString);
}
