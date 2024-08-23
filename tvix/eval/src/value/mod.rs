//! This module implements the backing representation of runtime
//! values in the Nix language.
use std::cmp::Ordering;
use std::fmt::Display;
use std::num::{NonZeroI32, NonZeroUsize};
use std::path::PathBuf;
use std::rc::Rc;

use bstr::{BString, ByteVec};
use codemap::Span;
use lexical_core::format::CXX_LITERAL;
use serde::Deserialize;

#[cfg(feature = "arbitrary")]
mod arbitrary;
mod attrs;
mod builtin;
mod function;
mod json;
mod list;
mod path;
mod string;
mod thunk;

use crate::errors::{CatchableErrorKind, ErrorKind};
use crate::opcode::StackIdx;
use crate::vm::generators::{self, GenCo};
use crate::AddContext;
pub use attrs::NixAttrs;
pub use builtin::{Builtin, BuiltinResult};
pub use function::Formals;
pub use function::{Closure, Lambda};
pub use list::NixList;
pub use path::canon_path;
pub use string::{NixContext, NixContextElement, NixString};
pub use thunk::Thunk;

pub use self::thunk::ThunkSet;

use lazy_static::lazy_static;

#[warn(variant_size_differences)]
#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(NixString),

    #[serde(skip)]
    Path(Box<PathBuf>),
    Attrs(Box<NixAttrs>),
    List(NixList),

    #[serde(skip)]
    Closure(Rc<Closure>), // must use Rc<Closure> here in order to get proper pointer equality

    #[serde(skip)]
    Builtin(Builtin),

    // Internal values that, while they technically exist at runtime,
    // are never returned to or created directly by users.
    #[serde(skip_deserializing)]
    Thunk(Thunk),

    // See [`compiler::compile_select_or()`] for explanation
    #[serde(skip)]
    AttrNotFound,

    // this can only occur in Chunk::Constants and nowhere else
    #[serde(skip)]
    Blueprint(Rc<Lambda>),

    #[serde(skip)]
    DeferredUpvalue(StackIdx),
    #[serde(skip)]
    UnresolvedPath(Box<PathBuf>),
    #[serde(skip)]
    Json(Box<(serde_json::Value, NixContext)>),

    #[serde(skip)]
    FinaliseRequest(bool),

    #[serde(skip)]
    Catchable(Box<CatchableErrorKind>),
}

impl From<CatchableErrorKind> for Value {
    #[inline]
    fn from(c: CatchableErrorKind) -> Self {
        Self::Catchable(Box::new(c))
    }
}

impl<V> From<Result<V, CatchableErrorKind>> for Value
where
    Self: From<V>,
{
    #[inline]
    fn from(v: Result<V, CatchableErrorKind>) -> Self {
        match v {
            Ok(v) => v.into(),
            Err(e) => Self::Catchable(Box::new(e)),
        }
    }
}

lazy_static! {
    static ref WRITE_FLOAT_OPTIONS: lexical_core::WriteFloatOptions =
        lexical_core::WriteFloatOptionsBuilder::new()
            .trim_floats(true)
            .round_mode(lexical_core::write_float_options::RoundMode::Round)
            .positive_exponent_break(Some(NonZeroI32::new(5).unwrap()))
            .max_significant_digits(Some(NonZeroUsize::new(6).unwrap()))
            .build()
            .unwrap();
}

// Helper macros to generate the to_*/as_* macros while accounting for
// thunks.

/// Generate an `as_*` method returning a reference to the expected
/// type, or a type error. This only works for types that implement
/// `Copy`, as returning a reference to an inner thunk value is not
/// possible.

/// Generate an `as_*/to_*` accessor method that returns either the
/// expected type, or a type error.
macro_rules! gen_cast {
    ( $name:ident, $type:ty, $expected:expr, $variant:pat, $result:expr ) => {
        pub fn $name(&self) -> Result<$type, ErrorKind> {
            match self {
                $variant => Ok($result),
                Value::Thunk(thunk) => Self::$name(&thunk.value()),
                other => Err(type_error($expected, &other)),
            }
        }
    };
}

/// Generate an `as_*_mut/to_*_mut` accessor method that returns either the
/// expected type, or a type error.
macro_rules! gen_cast_mut {
    ( $name:ident, $type:ty, $expected:expr, $variant:ident) => {
        pub fn $name(&mut self) -> Result<&mut $type, ErrorKind> {
            match self {
                Value::$variant(x) => Ok(x),
                other => Err(type_error($expected, &other)),
            }
        }
    };
}

/// Generate an `is_*` type-checking method.
macro_rules! gen_is {
    ( $name:ident, $variant:pat ) => {
        #[must_use]
        pub fn $name(&self) -> bool {
            match self {
                $variant => true,
                Value::Thunk(thunk) => Self::$name(&thunk.value()),
                _ => false,
            }
        }
    };
}

/// Describes what input types are allowed when coercing a `Value` to a string
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct CoercionKind {
    /// If false only coerce already "stringly" types like strings and paths, but
    /// also coerce sets that have a `__toString` attribute. In Tvix, this is
    /// usually called a weak coercion. Equivalent to passing `false` as the
    /// `coerceMore` argument of `EvalState::coerceToString` in C++ Nix.
    ///
    /// If true coerce all value types included by a weak coercion, but also
    /// coerce `null`, booleans, integers, floats and lists of coercible types.
    /// Consequently, we call this a strong coercion. Equivalent to passing
    /// `true` as `coerceMore` in C++ Nix.
    pub strong: bool,

    /// If `import_paths` is `true`, paths are imported into the store and their
    /// store path is the result of the coercion (equivalent to the
    /// `copyToStore` argument of `EvalState::coerceToString` in C++ Nix).
    pub import_paths: bool,
}

impl From<CoercionKind> for u8 {
    fn from(k: CoercionKind) -> Self {
        Self::from(k.strong) | Self::from(k.import_paths) << 1
    }
}

impl From<u8> for CoercionKind {
    fn from(byte: u8) -> Self {
        Self {
            strong: byte & 0x01 != 0,
            import_paths: byte & 0x02 != 0,
        }
    }
}

impl<T> From<T> for Value
where
    T: Into<NixString>,
{
    fn from(t: T) -> Self {
        Self::String(t.into())
    }
}

/// Constructors
impl Value {
    /// Construct a [`Value::Attrs`] from a [`NixAttrs`].
    #[must_use]
    pub fn attrs(attrs: NixAttrs) -> Self {
        Self::Attrs(Box::new(attrs))
    }
}

/// Controls what kind of by-pointer equality comparison is allowed.
///
/// See `//tvix/docs/value-pointer-equality.md` for details.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum PointerEquality {
    /// Pointer equality not allowed at all.
    ForbidAll,

    /// Pointer equality comparisons only allowed for nested values.
    AllowNested,

    /// Pointer equality comparisons are allowed in all contexts.
    AllowAll,
}

impl Value {
    /// Deeply forces a value, traversing e.g. lists and attribute sets and forcing
    /// their contents, too.
    ///
    /// This is a generator function.
    pub(super) async fn deep_force(self, co: GenCo, span: Span) -> Result<Self, ErrorKind> {
        if let Some(v) = Self::deep_force_(self.clone(), co, span).await? {
            Ok(v)
        } else {
            Ok(self)
        }
    }

    /// Returns Some(v) or None to indicate the returned value is myself
    async fn deep_force_(myself: Self, co: GenCo, span: Span) -> Result<Option<Self>, ErrorKind> {
        // This is a stack of values which still remain to be forced.
        let mut vals = vec![myself];

        let mut thunk_set: ThunkSet = Default::default();

        loop {
            let v = if let Some(v) = vals.pop() {
                v
            } else {
                return Ok(None);
            };

            // Get rid of any top-level thunks, and bail out of self-recursive
            // thunks.
            let value = if let Self::Thunk(t) = &v {
                if !thunk_set.insert(t) {
                    continue;
                }
                Thunk::force_(t.clone(), &co, span).await?
            } else {
                v
            };

            match value {
                // Short-circuit on already evaluated values, or fail on internal values.
                Self::Null
                | Self::Bool(_)
                | Self::Integer(_)
                | Self::Float(_)
                | Self::String(_)
                | Self::Path(_)
                | Self::Closure(_)
                | Self::Builtin(_) => continue,

                Self::List(list) => {
                    for val in list.into_iter().rev() {
                        vals.push(val);
                    }
                    continue;
                }

                Self::Attrs(attrs) => {
                    for (_, val) in attrs.into_iter().rev() {
                        vals.push(val);
                    }
                    continue;
                }

                Self::Thunk(_) => panic!("Tvix bug: force_value() returned a thunk"),

                Self::Catchable(_) => return Ok(Some(value)),

                Self::AttrNotFound
                | Self::Blueprint(_)
                | Self::DeferredUpvalue(_)
                | Self::UnresolvedPath(_)
                | Self::Json(..)
                | Self::FinaliseRequest(_) => panic!(
                    "Tvix bug: internal value left on stack: {}",
                    value.type_of()
                ),
            }
        }
    }

    pub async fn coerce_to_string(
        self,
        co: GenCo,
        kind: CoercionKind,
        span: Span,
    ) -> Result<Self, ErrorKind> {
        self.coerce_to_string_(&co, kind, span).await
    }

    /// Coerce a `Value` to a string. See `CoercionKind` for a rundown of what
    /// input types are accepted under what circumstances.
    pub async fn coerce_to_string_(
        self,
        co: &GenCo,
        kind: CoercionKind,
        span: Span,
    ) -> Result<Self, ErrorKind> {
        let mut result = BString::default();
        let mut vals = vec![self];
        // Track if we are coercing the first value of a list to correctly emit
        // separating white spaces.
        let mut is_list_head = None;
        // FIXME(raitobezarius): as per https://b.tvl.fyi/issues/364
        // we might be interested into more powerful context-related coercion kinds.
        let mut context: NixContext = NixContext::new();

        loop {
            let value = if let Some(v) = vals.pop() {
                v.force(co, span).await?
            } else {
                return Ok(Self::String(NixString::new_context_from(context, result)));
            };
            let coerced: Result<BString, _> = match (value, kind) {
                // coercions that are always done
                (Self::String(mut s), _) => {
                    if let Some(ctx) = s.take_context() {
                        context.extend(ctx.into_iter());
                    }
                    Ok((*s).into())
                }

                // TODO(sterni): Think about proper encoding handling here. This needs
                // general consideration anyways, since one current discrepancy between
                // C++ Nix and Tvix is that the former's strings are arbitrary byte
                // sequences without NUL bytes, whereas Tvix only allows valid
                // Unicode. See also b/189.
                (
                    Self::Path(p),
                    CoercionKind {
                        import_paths: true, ..
                    },
                ) => {
                    let imported = generators::request_path_import(co, *p).await;
                    // When we import a path from the evaluator, we must attach
                    // its original path as its context.
                    context = context.append(NixContextElement::Plain(
                        imported.to_string_lossy().to_string(),
                    ));
                    Ok(imported.into_os_string().into_encoded_bytes().into())
                }
                (
                    Self::Path(p),
                    CoercionKind {
                        import_paths: false,
                        ..
                    },
                ) => Ok(p.into_os_string().into_encoded_bytes().into()),

                // Attribute sets can be converted to strings if they either have an
                // `__toString` attribute which holds a function that receives the
                // set itself or an `outPath` attribute which should be a string.
                // `__toString` is preferred.
                (Self::Attrs(attrs), kind) => {
                    if let Some(to_string) = attrs.select("__toString") {
                        let callable = to_string.clone().force(co, span).await?;

                        // Leave the attribute set on the stack as an argument
                        // to the function call.
                        generators::request_stack_push(co, Self::Attrs(attrs.clone())).await;

                        // Call the callable ...
                        let result = generators::request_call(co, callable).await;

                        // Recurse on the result, as attribute set coercion
                        // actually works recursively, e.g. you can even return
                        // /another/ set with a __toString attr.
                        vals.push(result);
                        continue;
                    } else if let Some(out_path) = attrs.select("outPath") {
                        vals.push(out_path.clone());
                        continue;
                    } else {
                        return Err(ErrorKind::NotCoercibleToString { from: "set", kind });
                    }
                }

                // strong coercions
                (Self::Null | Self::Bool(false), CoercionKind { strong: true, .. }) => {
                    Ok("".into())
                }
                (Self::Bool(true), CoercionKind { strong: true, .. }) => Ok("1".into()),

                (Self::Integer(i), CoercionKind { strong: true, .. }) => Ok(format!("{i}").into()),
                (Self::Float(f), CoercionKind { strong: true, .. }) => {
                    // contrary to normal Display, coercing a float to a string will
                    // result in unconditional 6 decimal places
                    Ok(format!("{f:.6}").into())
                }

                // Lists are coerced by coercing their elements and interspersing spaces
                (Self::List(list), CoercionKind { strong: true, .. }) => {
                    for elem in list.into_iter().rev() {
                        vals.push(elem);
                    }
                    // In case we are coercing a list within a list we don't want
                    // to touch this. Since the algorithm is nonrecursive, the
                    // space would not have been created yet (due to continue).
                    if is_list_head.is_none() {
                        is_list_head = Some(true);
                    }
                    continue;
                }

                (Self::Thunk(_), _) => panic!("Tvix bug: force returned unforced thunk"),

                val @ (
                    Self::Closure(_)
                    | Self::Builtin(_)
                    | Self::Null
                    | Self::Bool(_)
                    | Self::Integer(_)
                    | Self::Float(_)
                    | Self::List(_),
                    _,
                ) => Err(ErrorKind::NotCoercibleToString {
                    from: val.0.type_of(),
                    kind,
                }),

                (c @ Self::Catchable(_), _) => return Ok(c),

                (
                    Self::AttrNotFound
                    | Self::Blueprint(_)
                    | Self::DeferredUpvalue(_)
                    | Self::UnresolvedPath(_)
                    | Self::Json(..)
                    | Self::FinaliseRequest(_),
                    _,
                ) => {
                    panic!("tvix bug: .coerce_to_string() called on internal value")
                }
            };

            if let Some(head) = is_list_head {
                if !head {
                    result.push(b' ');
                } else {
                    is_list_head = Some(false);
                }
            }

            result.push_str(&coerced?);
        }
    }

    pub(crate) async fn nix_eq_owned_genco(
        self,
        other: Self,
        co: GenCo,
        ptr_eq: PointerEquality,
        span: Span,
    ) -> Result<Self, ErrorKind> {
        self.nix_eq(other, &co, ptr_eq, span).await
    }

    /// Compare two Nix values for equality, forcing nested parts of the structure
    /// as needed.
    ///
    /// This comparison needs to be invoked for nested values (e.g. in lists and
    /// attribute sets) as well, which is done by suspending and asking the VM to
    /// perform the nested comparison.
    ///
    /// The `top_level` parameter controls whether this invocation is the top-level
    /// comparison, or a nested value comparison. See
    /// `//tvix/docs/value-pointer-equality.md`
    pub(crate) async fn nix_eq(
        self,
        other: Self,
        co: &GenCo,
        ptr_eq: PointerEquality,
        span: Span,
    ) -> Result<Self, ErrorKind> {
        // this is a stack of ((v1,v2),peq) triples to be compared;
        // after each triple is popped off of the stack, v1 is
        // compared to v2 using peq-mode PointerEquality
        let mut vals = vec![((self, other), ptr_eq)];

        loop {
            let ((a, b), ptr_eq) = if let Some(abp) = vals.pop() {
                abp
            } else {
                // stack is empty, so comparison has succeeded
                return Ok(Self::Bool(true));
            };
            let a = match a {
                Self::Thunk(thunk) => {
                    // If both values are thunks, and thunk comparisons are allowed by
                    // pointer, do that and move on.
                    if ptr_eq == PointerEquality::AllowAll {
                        if let Self::Thunk(t1) = &b {
                            if t1.ptr_eq(&thunk) {
                                continue;
                            }
                        }
                    };

                    Thunk::force_(thunk, co, span).await?
                }

                _ => a,
            };

            let b = b.force(co, span).await?;

            debug_assert!(!matches!(a, Self::Thunk(_)));
            debug_assert!(!matches!(b, Self::Thunk(_)));

            let result = match (a, b) {
                // Trivial comparisons
                (c @ Self::Catchable(_), _) => return Ok(c),
                (_, c @ Self::Catchable(_)) => return Ok(c),
                (Self::Null, Self::Null) => true,
                (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
                (Self::String(s1), Self::String(s2)) => s1 == s2,
                (Self::Path(p1), Self::Path(p2)) => p1 == p2,

                // Numerical comparisons (they work between float & int)
                (Self::Integer(i1), Self::Integer(i2)) => i1 == i2,
                (Self::Integer(i), Self::Float(f)) => i as f64 == f,
                (Self::Float(f1), Self::Float(f2)) => f1 == f2,
                (Self::Float(f), Self::Integer(i)) => i as f64 == f,

                // List comparisons
                (Self::List(l1), Self::List(l2)) => {
                    if ptr_eq >= PointerEquality::AllowNested && l1.ptr_eq(&l2) {
                        continue;
                    }

                    if l1.len() != l2.len() {
                        return Ok(Self::Bool(false));
                    }

                    vals.extend(l1.into_iter().rev().zip(l2.into_iter().rev()).zip(
                        std::iter::repeat(std::cmp::max(ptr_eq, PointerEquality::AllowNested)),
                    ));
                    continue;
                }

                (_, Self::List(_)) | (Self::List(_), _) => return Ok(Self::Bool(false)),

                // Attribute set comparisons
                (Self::Attrs(a1), Self::Attrs(a2)) => {
                    if ptr_eq >= PointerEquality::AllowNested && a1.ptr_eq(&a2) {
                        continue;
                    }

                    // Special-case for derivation comparisons: If both attribute sets
                    // have `type = derivation`, compare them by `outPath`.
                    #[allow(clippy::single_match)] // might need more match arms later
                    match (a1.select("type"), a2.select("type")) {
                        (Some(v1), Some(v2)) => {
                            let s1 = v1.clone().force(co, span).await?;
                            if s1.is_catchable() {
                                return Ok(s1);
                            }
                            let s2 = v2.clone().force(co, span).await?;
                            if s2.is_catchable() {
                                return Ok(s2);
                            }
                            let s1 = s1.to_str();
                            let s2 = s2.to_str();

                            if let (Ok(s1), Ok(s2)) = (s1, s2) {
                                if s1 == "derivation" && s2 == "derivation" {
                                    // TODO(tazjin): are the outPaths really required,
                                    // or should it fall through?
                                    let out1 = a1
                                        .select_required("outPath")
                                        .context("comparing derivations")?
                                        .clone();

                                    let out2 = a2
                                        .select_required("outPath")
                                        .context("comparing derivations")?
                                        .clone();

                                    let out1 = out1.clone().force(co, span).await?;
                                    let out2 = out2.clone().force(co, span).await?;

                                    if out1.is_catchable() {
                                        return Ok(out1);
                                    }

                                    if out2.is_catchable() {
                                        return Ok(out2);
                                    }

                                    let result =
                                        out1.to_contextful_str()? == out2.to_contextful_str()?;
                                    if !result {
                                        return Ok(Self::Bool(false));
                                    } else {
                                        continue;
                                    }
                                }
                            }
                        }
                        _ => {}
                    };

                    if a1.len() != a2.len() {
                        return Ok(Self::Bool(false));
                    }

                    // note that it is important to be careful here with the
                    // order we push the keys and values in order to properly
                    // compare attrsets containing `throw` elements.
                    let iter1 = a1.into_iter_sorted().rev();
                    let iter2 = a2.into_iter_sorted().rev();
                    for ((k1, v1), (k2, v2)) in iter1.zip(iter2) {
                        vals.push((
                            (v1, v2),
                            std::cmp::max(ptr_eq, PointerEquality::AllowNested),
                        ));
                        vals.push((
                            (k1.into(), k2.into()),
                            std::cmp::max(ptr_eq, PointerEquality::AllowNested),
                        ));
                    }
                    continue;
                }

                (Self::Attrs(_), _) | (_, Self::Attrs(_)) => return Ok(Self::Bool(false)),

                (Self::Closure(c1), Self::Closure(c2))
                    if ptr_eq >= PointerEquality::AllowNested =>
                {
                    if Rc::ptr_eq(&c1, &c2) {
                        continue;
                    } else {
                        return Ok(Self::Bool(false));
                    }
                }

                // Everything else is either incomparable (e.g. internal types) or
                // false.
                _ => return Ok(Self::Bool(false)),
            };
            if !result {
                return Ok(Self::Bool(false));
            }
        }
    }

    #[must_use]
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::Null => "null",
            Self::Bool(_) => "bool",
            Self::Integer(_) => "int",
            Self::Float(_) => "float",
            Self::String(_) => "string",
            Self::Path(_) => "path",
            Self::Attrs(_) => "set",
            Self::List(_) => "list",
            Self::Closure(_) | Self::Builtin(_) => "lambda",

            // Internal types. Note: These are only elaborated here
            // because it makes debugging easier. If a user ever sees
            // any of these strings, it's a bug.
            Self::Thunk(_) => "internal[thunk]",
            Self::AttrNotFound => "internal[attr_not_found]",
            Self::Blueprint(_) => "internal[blueprint]",
            Self::DeferredUpvalue(_) => "internal[deferred_upvalue]",
            Self::UnresolvedPath(_) => "internal[unresolved_path]",
            Self::Json(..) => "internal[json]",
            Self::FinaliseRequest(_) => "internal[finaliser_sentinel]",
            Self::Catchable(_) => "internal[catchable]",
        }
    }

    gen_cast!(as_bool, bool, "bool", Self::Bool(b), *b);
    gen_cast!(as_int, i64, "int", Self::Integer(x), *x);
    gen_cast!(as_float, f64, "float", Self::Float(x), *x);

    /// Cast the current value into a **context-less** string.
    /// If you wanted to cast it into a potentially contextful string,
    /// you have to explicitly use `to_contextful_str`.
    /// Contextful strings are special, they should not be obtained
    /// everytime you want a string.
    pub fn to_str(&self) -> Result<NixString, ErrorKind> {
        match self {
            Self::String(s) if !s.has_context() => Ok((*s).clone()),
            Self::Thunk(thunk) => Self::to_str(&thunk.value()),
            other => Err(type_error("contextless strings", other)),
        }
    }

    gen_cast!(
        to_contextful_str,
        NixString,
        "contextful string",
        Self::String(s),
        (*s).clone()
    );
    gen_cast!(to_path, Box<PathBuf>, "path", Self::Path(p), p.clone());
    gen_cast!(to_attrs, Box<NixAttrs>, "set", Self::Attrs(a), a.clone());
    gen_cast!(to_list, NixList, "list", Self::List(l), l.clone());
    gen_cast!(
        as_closure,
        Rc<Closure>,
        "lambda",
        Self::Closure(c),
        c.clone()
    );

    gen_cast_mut!(as_list_mut, NixList, "list", List);

    gen_is!(is_path, Self::Path(_));
    gen_is!(is_number, Self::Integer(_) | Self::Float(_));
    gen_is!(is_bool, Self::Bool(_));
    gen_is!(is_attrs, Self::Attrs(_));
    gen_is!(is_catchable, Self::Catchable(_));

    /// Returns `true` if the value is a [`Thunk`].
    ///
    /// [`Thunk`]: Value::Thunk
    #[must_use]
    pub fn is_thunk(&self) -> bool {
        matches!(self, Self::Thunk(..))
    }

    /// Compare `self` against other using (fallible) Nix ordering semantics.
    ///
    /// The function is intended to be used from within other generator
    /// functions or `gen!` blocks.
    pub async fn nix_cmp_ordering(
        self,
        other: Self,
        co: GenCo,
        span: Span,
    ) -> Result<Result<Ordering, CatchableErrorKind>, ErrorKind> {
        Self::nix_cmp_ordering_(self, other, co, span).await
    }

    async fn nix_cmp_ordering_(
        myself: Self,
        other: Self,
        co: GenCo,
        span: Span,
    ) -> Result<Result<Ordering, CatchableErrorKind>, ErrorKind> {
        // this is a stack of ((v1,v2),peq) triples to be compared;
        // after each triple is popped off of the stack, v1 is
        // compared to v2 using peq-mode PointerEquality
        let mut vals = vec![((myself, other), PointerEquality::ForbidAll)];

        loop {
            let ((mut a, mut b), ptr_eq) = if let Some(abp) = vals.pop() {
                abp
            } else {
                // stack is empty, so they are equal
                return Ok(Ok(Ordering::Equal));
            };
            if ptr_eq == PointerEquality::AllowAll {
                if a.clone()
                    .nix_eq(b.clone(), &co, PointerEquality::AllowAll, span)
                    .await?
                    .as_bool()?
                {
                    continue;
                }
                a = a.force(&co, span).await?;
                b = b.force(&co, span).await?;
            }
            let result = match (a, b) {
                (Self::Catchable(c), _) => return Ok(Err(*c)),
                (_, Self::Catchable(c)) => return Ok(Err(*c)),
                // same types
                (Self::Integer(i1), Self::Integer(i2)) => i1.cmp(&i2),
                (Self::Float(f1), Self::Float(f2)) => f1.total_cmp(&f2),
                (Self::String(s1), Self::String(s2)) => s1.cmp(&s2),
                (Self::List(l1), Self::List(l2)) => {
                    let max = l1.len().max(l2.len());
                    for j in 0..max {
                        let i = max - 1 - j;
                        if i >= l2.len() {
                            vals.push(((1.into(), 0.into()), PointerEquality::ForbidAll));
                        } else if i >= l1.len() {
                            vals.push(((0.into(), 1.into()), PointerEquality::ForbidAll));
                        } else {
                            vals.push(((l1[i].clone(), l2[i].clone()), PointerEquality::AllowAll));
                        }
                    }
                    continue;
                }

                // different types
                (Self::Integer(i1), Self::Float(f2)) => (i1 as f64).total_cmp(&f2),
                (Self::Float(f1), Self::Integer(i2)) => f1.total_cmp(&(i2 as f64)),

                // unsupported types
                (lhs, rhs) => {
                    return Err(ErrorKind::Incomparable {
                        lhs: lhs.type_of(),
                        rhs: rhs.type_of(),
                    })
                }
            };
            if result != Ordering::Equal {
                return Ok(Ok(result));
            }
        }
    }

    // TODO(amjoseph): de-asyncify this (when called directly by the VM)
    pub async fn force(self, co: &GenCo, span: Span) -> Result<Self, ErrorKind> {
        if let Self::Thunk(thunk) = self {
            // TODO(amjoseph): use #[tailcall::mutual]
            return Thunk::force_(thunk, co, span).await;
        }
        Ok(self)
    }

    // need two flavors, because async
    pub async fn force_owned_genco(self, co: GenCo, span: Span) -> Result<Self, ErrorKind> {
        if let Self::Thunk(thunk) = self {
            // TODO(amjoseph): use #[tailcall::mutual]
            return Thunk::force_(thunk, &co, span).await;
        }
        Ok(self)
    }

    /// Explain a value in a human-readable way, e.g. by presenting
    /// the docstrings of functions if present.
    #[must_use]
    pub fn explain(&self) -> String {
        match self {
            Self::Null => "the 'null' value".into(),
            Self::Bool(b) => format!("the boolean value '{b}'"),
            Self::Integer(i) => format!("the integer '{i}'"),
            Self::Float(f) => format!("the float '{f}'"),
            Self::String(s) if s.has_context() => format!("the contextful string '{s}'"),
            Self::String(s) => format!("the contextless string '{s}'"),
            Self::Path(p) => format!("the path '{}'", p.to_string_lossy()),
            Self::Attrs(attrs) => format!("a {}-item attribute set", attrs.len()),
            Self::List(list) => format!("a {}-item list", list.len()),

            Self::Closure(f) => {
                if let Some(name) = &f.lambda.name {
                    format!("the user-defined Nix function '{name}'")
                } else {
                    "a user-defined Nix function".to_string()
                }
            }

            Self::Builtin(b) => {
                let mut out = format!("the builtin function '{}'", b.name());
                if let Some(docs) = b.documentation() {
                    out.push_str("\n\n");
                    out.push_str(docs);
                }
                out
            }

            // TODO: handle suspended thunks with a different explanation instead of panicking
            Self::Thunk(t) => t.value().explain(),

            Self::Catchable(_) => "a catchable failure".into(),

            Self::AttrNotFound
            | Self::Blueprint(_)
            | Self::DeferredUpvalue(_)
            | Self::UnresolvedPath(_)
            | Self::Json(..)
            | Self::FinaliseRequest(_) => "an internal Tvix evaluator value".into(),
        }
    }
}

trait TotalDisplay {
    fn total_fmt(&self, f: &mut std::fmt::Formatter<'_>, set: &mut ThunkSet) -> std::fmt::Result;
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.total_fmt(f, &mut Default::default())
    }
}

/// Emulates the C++-Nix style formatting of floats, which diverges
/// significantly from Rust's native float formatting.
fn total_fmt_float<F: std::fmt::Write>(num: f64, mut f: F) -> std::fmt::Result {
    let mut buf = [b'0'; lexical_core::BUFFER_SIZE];
    let mut s = lexical_core::write_with_options::<f64, { CXX_LITERAL }>(
        num,
        &mut buf,
        &WRITE_FLOAT_OPTIONS,
    );

    // apply some postprocessing on the buffer. If scientific
    // notation is used (we see an `e`), and the next character is
    // a digit, add the missing `+` sign.)
    let mut new_s = Vec::with_capacity(s.len());

    if s.contains(&b'e') {
        for (i, c) in s.iter().enumerate() {
            // encountered `e`
            if c == &b'e' {
                // next character is a digit (so no negative exponent)
                if s.len() > i && s[i + 1].is_ascii_digit() {
                    // copy everything from the start up to (including) the e
                    new_s.extend_from_slice(&s[0..=i]);
                    // add the missing '+'
                    new_s.push(b'+');
                    // check for the remaining characters.
                    // If it's only one, we need to prepend a trailing zero
                    if s.len() == i + 2 {
                        new_s.push(b'0');
                    }
                    new_s.extend_from_slice(&s[i + 1..]);
                    break;
                }
            }
        }

        // if we modified the scientific notation, flip the reference
        if !new_s.is_empty() {
            s = &mut new_s;
        }
    } else if s.contains(&b'.') {
        // else, if this is not scientific notation, and there's a
        // decimal point, make sure we really drop trailing zeroes.
        // In some cases, lexical_core doesn't.
        for (i, c) in s.iter().enumerate() {
            // at `.``
            if c == &b'.' {
                // trim zeroes from the right side.
                let frac = String::from_utf8_lossy(&s[i + 1..]);
                let frac_no_trailing_zeroes = frac.trim_end_matches('0');

                if frac.len() != frac_no_trailing_zeroes.len() {
                    // we managed to strip something, construct new_s
                    if frac_no_trailing_zeroes.is_empty() {
                        // if frac_no_trailing_zeroes is empty, the fractional part was all zeroes, so we can drop the decimal point as well
                        new_s.extend_from_slice(&s[0..i]);
                    } else {
                        // else, assemble the rest of the string
                        new_s.extend_from_slice(&s[0..=i]);
                        new_s.extend_from_slice(frac_no_trailing_zeroes.as_bytes());
                    }

                    // flip the reference
                    s = &mut new_s;
                    break;
                }
            }
        }
    }

    write!(f, "{}", String::from_utf8_lossy(s))
}

impl TotalDisplay for Value {
    fn total_fmt(&self, f: &mut std::fmt::Formatter<'_>, set: &mut ThunkSet) -> std::fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Bool(true) => f.write_str("true"),
            Self::Bool(false) => f.write_str("false"),
            Self::Integer(num) => write!(f, "{num}"),
            Self::String(s) => s.fmt(f),
            Self::Path(p) => p.display().fmt(f),
            Self::Attrs(attrs) => attrs.total_fmt(f, set),
            Self::List(list) => list.total_fmt(f, set),
            // TODO: fancy REPL display with position
            Self::Closure(_) => f.write_str("<LAMBDA>"),
            Self::Builtin(builtin) => builtin.fmt(f),

            // Nix prints floats with a maximum precision of 5 digits
            // only. Except when it decides to use scientific notation
            // (with a + after the `e`, and zero-padded to 0 digits)
            Self::Float(num) => total_fmt_float(*num, f),

            // internal types
            Self::AttrNotFound => f.write_str("internal[not found]"),
            Self::Blueprint(_) => f.write_str("internal[blueprint]"),
            Self::DeferredUpvalue(_) => f.write_str("internal[deferred_upvalue]"),
            Self::UnresolvedPath(_) => f.write_str("internal[unresolved_path]"),
            Self::Json(..) => f.write_str("internal[json]"),
            Self::FinaliseRequest(_) => f.write_str("internal[finaliser_sentinel]"),

            // Delegate thunk display to the type, as it must handle
            // the case of already evaluated or cyclic thunks.
            Self::Thunk(t) => t.total_fmt(f, set),
            Self::Catchable(_) => panic!("total_fmt() called on a CatchableErrorKind"),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Self::Integer(i)
    }
}

impl From<f64> for Value {
    fn from(i: f64) -> Self {
        Self::Float(i)
    }
}

impl From<PathBuf> for Value {
    fn from(path: PathBuf) -> Self {
        Self::Path(Box::new(path))
    }
}

fn type_error(expected: &'static str, actual: &Value) -> ErrorKind {
    ErrorKind::TypeError {
        expected,
        actual: actual.type_of(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<Value>(), 16);
    }

    mod floats {
        use crate::value::total_fmt_float;

        #[test]
        fn format_float() {
            let ff = [
                (0f64, "0"),
                (1.0f64, "1"),
                (-0.01, "-0.01"),
                (5e+22, "5e+22"),
                (1e6, "1e+06"),
                (-2E-2, "-0.02"),
                (6.626e-34, "6.626e-34"),
                (9_224_617.445_991_227, "9.22462e+06"),
            ];
            for (n, expected) in &ff {
                let mut buf = String::new();
                let res = total_fmt_float(*n, &mut buf);
                assert!(res.is_ok());
                assert_eq!(
                    expected, &buf,
                    "{} should be formatted as {}, but got {}",
                    n, expected, &buf
                );
            }
        }
    }
}
