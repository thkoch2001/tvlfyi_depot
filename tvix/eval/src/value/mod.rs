//! This module implements the backing representation of runtime
//! values in the Nix language.
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};
use std::mem::ManuallyDrop;
use std::num::{NonZeroI32, NonZeroUsize};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use bstr::{BString, ByteVec};
use erasable::Erasable;
use lexical_core::format::CXX_LITERAL;
use serde::Deserialize;

#[cfg(feature = "arbitrary")]
mod arbitrary;
mod attrs;
mod builtin;
mod function;
mod json;
mod list;
mod nan_boxing;
mod path;
mod string;
mod thunk;

use crate::errors::{CatchableErrorKind, ErrorKind};
use crate::opcode::StackIdx;
use crate::spans::LightSpan;
use crate::vm::generators::{self, GenCo};
use crate::AddContext;
pub use attrs::NixAttrs;
pub use builtin::{Builtin, BuiltinResult};
pub(crate) use function::Formals;
pub use function::{Closure, Lambda};
pub use list::NixList;
pub use path::canon_path;
pub use string::{NixContext, NixContextElement, NixString};
pub use thunk::Thunk;

use self::nan_boxing::{BoxedNan, BoxedNanKind};
use self::string::{StringRef, StringRefMut};
pub use self::thunk::ThunkSet;

use lazy_static::lazy_static;

#[derive(Clone)]
enum Object {
    BigInt(i64),
    Path(PathBuf),
    // must use Rc<Closure> here in order to get proper pointer equality
    Closure(Rc<Closure>),
    // Internal values that, while they technically exist at runtime,
    // are never returned to or created directly by users.
    Builtin(Builtin),
    Thunk(Thunk),

    /// See [`compiler::compile_select_or()`] for explanation
    AttrNotFound,
    Blueprint(Rc<Lambda>),
    DeferredUpvalue(StackIdx),
    UnresolvedPath(PathBuf),
    Json(serde_json::Value),
    FinaliseRequest(bool),
    Catchable(CatchableErrorKind),
}

#[derive(Clone, Deserialize)]
#[serde(from = "V")]
pub struct Value(BoxedNan<NixString, Box<NixAttrs>, Box<NixList>, Box<Object>>);

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.match_ref())
    }
}

macro_rules! object_constructors {
    () => {};
    ($name: ident($type_and_variant : ident); $($args:tt)*) => (
        object_constructors!($name($type_and_variant, $type_and_variant););
        object_constructors!($($args)*);
    );
    ($name: ident($type : ty, $variant:ident); $($args:tt)*) => (
        pub fn $name<T>(x : T) -> Self
        where
            T: Into<$type>
        {
            Self::object(Object::$variant(x.into()))
        }
        object_constructors!($($args)*);
    );
}

/// Constructors
impl Value {
    pub const NULL: Self = Self(BoxedNan::NULL);

    pub fn bool(b: bool) -> Self {
        Self(BoxedNan::bool(b))
    }

    pub fn integer(i: i64) -> Self {
        if let Ok(i) = i32::try_from(i) {
            Self(BoxedNan::int(i))
        } else {
            Self::object(Object::BigInt(i))
        }
    }

    pub fn float(f: f64) -> Self {
        Self(BoxedNan::float(f))
    }

    pub fn string<T>(s: T) -> Self
    where
        T: Into<NixString>,
    {
        Self(BoxedNan::ptra(s.into()))
    }

    pub fn attrs<T>(a: T) -> Self
    where
        T: Into<NixAttrs>,
    {
        Self(BoxedNan::ptrb(Box::new(a.into())))
    }

    pub fn list<T>(l: T) -> Self
    where
        T: Into<NixList>,
    {
        Self(BoxedNan::ptrc(Box::new(l.into())))
    }

    pub fn attr_not_found() -> Self {
        // TODO: use a constant pointer for this, but avoid dropping
        Self::object(Object::AttrNotFound)
    }

    fn object(obj: Object) -> Self {
        Self(BoxedNan::ptrd(Box::new(obj)))
    }

    object_constructors! {
        closure(Rc<Closure>, Closure);
        path(PathBuf, Path);
        builtin(Builtin);
        thunk(Thunk);
        blueprint(Rc<Lambda>, Blueprint);
        deferred_upvalue(StackIdx, DeferredUpvalue);
        unresolved_path(PathBuf, UnresolvedPath);
        json(serde_json::Value, Json);
        finalise_request(bool, FinaliseRequest);
        catchable(CatchableErrorKind, Catchable);
    }
}

macro_rules! object_into {
    () => {};
    ($name:ident($variant:ident) -> $ret:ty; $($args:tt)*) => {
        pub fn $name(self) -> Result<$ret, Self> {
            match self.into_object()? {
                Object::$variant(x) => Ok(x),
                obj => Err(Self::object(obj)),
            }
        }

        object_into!($($args)*);
    };
}

/// Conversion
impl Value {
    pub fn into_string(self) -> Result<NixString, Self> {
        self.0.into_ptra().map_err(Self)
    }

    fn into_object(self) -> Result<Object, Self> {
        self.0.into_ptrd().map(|obj| *obj).map_err(Self)
    }

    object_into! {
        into_thunk(Thunk) -> Thunk;
        into_path(Path) -> PathBuf;
        into_catchable(Catchable) -> CatchableErrorKind;
        into_json_value(Json) -> serde_json::Value;
    }

    #[inline(always)]
    pub fn into_match(self) -> V {
        match self.0.kind() {
            BoxedNanKind::Null => V::Null,
            BoxedNanKind::Bool => V::Bool(unsafe { self.0.as_bool_unchecked() }),
            BoxedNanKind::Int => V::Integer(unsafe { self.0.as_int_unchecked() }.into()),
            BoxedNanKind::Float => V::Float(unsafe { self.0.as_float_unchecked() }),
            BoxedNanKind::PtrA => V::String(unsafe { self.0.into_ptra_unchecked() }),
            BoxedNanKind::PtrB => V::Attrs(*unsafe { self.0.into_ptrb_unchecked() }),
            BoxedNanKind::PtrC => V::List(*unsafe { self.0.into_ptrc_unchecked() }),
            BoxedNanKind::PtrD => match *unsafe { self.0.into_ptrd_unchecked() } {
                Object::BigInt(i) => V::Integer(i),
                Object::Path(p) => V::Path(p),
                Object::Closure(c) => V::Closure(c),
                Object::Builtin(b) => V::Builtin(b),
                Object::Thunk(x) => V::Thunk(x),
                Object::AttrNotFound => V::AttrNotFound,
                Object::Blueprint(x) => V::Blueprint(x),
                Object::DeferredUpvalue(x) => V::DeferredUpvalue(x),
                Object::UnresolvedPath(x) => V::UnresolvedPath(x),
                Object::Json(x) => V::Json(x),
                Object::FinaliseRequest(x) => V::FinaliseRequest(x),
                Object::Catchable(x) => V::Catchable(x),
            },
        }
    }

    #[inline(always)]
    pub fn match_ref(&self) -> VRef {
        match self.0.kind() {
            BoxedNanKind::Null => VRef::Null,
            BoxedNanKind::Bool => VRef::Bool(unsafe { self.0.as_bool_unchecked() }),
            BoxedNanKind::Int => VRef::Integer(unsafe { self.0.as_int_unchecked() }.into()),
            BoxedNanKind::Float => VRef::Float(unsafe { self.0.as_float_unchecked() }),
            BoxedNanKind::PtrA => VRef::String(unsafe {
                ManuallyDrop::new(NixString::unerase(self.0.as_ptr_unchecked()))
                    .as_ref()
                    .as_string_ref()
            }),
            BoxedNanKind::PtrB => VRef::Attrs(unsafe { self.0.as_ref_b_unchecked() }),
            BoxedNanKind::PtrC => VRef::List(unsafe { self.0.as_ref_c_unchecked() }),
            BoxedNanKind::PtrD => match unsafe { self.0.as_ref_d_unchecked() } {
                Object::BigInt(i) => VRef::Integer(*i),
                Object::Path(p) => VRef::Path(p),
                Object::Closure(c) => VRef::Closure(c),
                Object::Builtin(b) => VRef::Builtin(b),
                Object::Thunk(x) => VRef::Thunk(x),
                Object::AttrNotFound => VRef::AttrNotFound,
                Object::Blueprint(x) => VRef::Blueprint(x),
                Object::DeferredUpvalue(x) => VRef::DeferredUpvalue(*x),
                Object::UnresolvedPath(x) => VRef::UnresolvedPath(x),
                Object::Json(x) => VRef::Json(x),
                Object::FinaliseRequest(x) => VRef::FinaliseRequest(*x),
                Object::Catchable(x) => VRef::Catchable(x),
            },
        }
    }

    #[inline(always)]
    pub fn match_mut(&mut self) -> VMut {
        match self.0.kind() {
            BoxedNanKind::Null => VMut::Null,
            BoxedNanKind::Bool => VMut::Bool(unsafe { self.0.as_bool_unchecked() }),
            BoxedNanKind::Int => VMut::Integer(unsafe { self.0.as_int_unchecked() }.into()),
            BoxedNanKind::Float => VMut::Float(unsafe { self.0.as_float_unchecked() }),
            BoxedNanKind::PtrA => VMut::String(unsafe {
                ManuallyDrop::new(NixString::unerase(self.0.as_ptr_unchecked()))
                    .as_mut()
                    .as_string_ref_mut()
            }),
            BoxedNanKind::PtrB => VMut::Attrs(unsafe { self.0.as_mut_b_unchecked() }),
            BoxedNanKind::PtrC => VMut::List(unsafe { self.0.as_mut_c_unchecked() }),
            BoxedNanKind::PtrD => match unsafe { self.0.as_mut_d_unchecked() } {
                Object::BigInt(i) => VMut::Integer(*i),
                Object::Path(p) => VMut::Path(p),
                Object::Closure(c) => VMut::Closure(c),
                Object::Builtin(b) => VMut::Builtin(b),
                Object::Thunk(x) => VMut::Thunk(x),
                Object::AttrNotFound => VMut::AttrNotFound,
                Object::Blueprint(x) => VMut::Blueprint(x),
                Object::DeferredUpvalue(x) => VMut::DeferredUpvalue(x),
                Object::UnresolvedPath(x) => VMut::UnresolvedPath(x),
                Object::Json(x) => VMut::Json(x),
                Object::FinaliseRequest(x) => VMut::FinaliseRequest(x),
                Object::Catchable(x) => VMut::Catchable(x),
            },
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum V {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(NixString),
    #[serde(skip)]
    Path(PathBuf),
    Attrs(NixAttrs),
    List(NixList),
    #[serde(skip)]
    Closure(Rc<Closure>),
    #[serde(skip)]
    Builtin(Builtin),
    #[serde(skip)]
    Thunk(Thunk),
    #[serde(skip)]
    AttrNotFound,
    #[serde(skip)]
    Blueprint(Rc<Lambda>),
    #[serde(skip)]
    DeferredUpvalue(StackIdx),
    #[serde(skip)]
    UnresolvedPath(PathBuf),
    #[serde(skip)]
    Json(serde_json::Value),
    #[serde(skip)]
    FinaliseRequest(bool),
    #[serde(skip)]
    Catchable(CatchableErrorKind),
}

impl V {
    pub fn type_of(&self) -> &'static str {
        self.as_ref().type_of()
    }

    pub fn as_ref(&self) -> VRef {
        match self {
            V::Null => VRef::Null,
            V::Bool(x) => VRef::Bool(*x),
            V::Integer(x) => VRef::Integer(*x),
            V::Float(x) => VRef::Float(*x),
            V::String(x) => VRef::String(x.as_string_ref()),
            V::Path(x) => VRef::Path(x),
            V::Attrs(x) => VRef::Attrs(x),
            V::List(x) => VRef::List(x),
            V::Closure(x) => VRef::Closure(x),
            V::Builtin(x) => VRef::Builtin(x),
            V::Thunk(x) => VRef::Thunk(x),
            V::AttrNotFound => VRef::AttrNotFound,
            V::Blueprint(x) => VRef::Blueprint(x),
            V::DeferredUpvalue(x) => VRef::DeferredUpvalue(*x),
            V::UnresolvedPath(x) => VRef::UnresolvedPath(x),
            V::Json(x) => VRef::Json(x),
            V::FinaliseRequest(x) => VRef::FinaliseRequest(*x),
            V::Catchable(x) => VRef::Catchable(x),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum VRef<'a> {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(StringRef<'a>),
    Path(&'a PathBuf),
    Attrs(&'a NixAttrs),
    List(&'a NixList),
    Closure(&'a Rc<Closure>),
    Builtin(&'a Builtin),
    Thunk(&'a Thunk),
    AttrNotFound,
    Blueprint(&'a Rc<Lambda>),
    DeferredUpvalue(StackIdx),
    UnresolvedPath(&'a Path),
    Json(&'a serde_json::Value),
    FinaliseRequest(bool),
    Catchable(&'a CatchableErrorKind),
}

impl VRef<'_> {
    pub fn type_of(&self) -> &'static str {
        match self {
            VRef::Null => "null",
            VRef::Bool(_) => "bool",
            VRef::Integer(_) => "int",
            VRef::Float(_) => "float",
            VRef::String(_) => "string",
            VRef::Path(_) => "path",
            VRef::Attrs(_) => "set",
            VRef::List(_) => "list",
            VRef::Closure(_) | VRef::Builtin(_) => "lambda",

            // Internal types. Note: These are only elaborated here
            // because it makes debugging easier. If a user ever sees
            // any of these strings, it's a bug.
            VRef::Thunk(_) => "internal[thunk]",
            VRef::AttrNotFound => "internal[attr_not_found]",
            VRef::Blueprint(_) => "internal[blueprint]",
            VRef::DeferredUpvalue(_) => "internal[deferred_upvalue]",
            VRef::UnresolvedPath(_) => "internal[unresolved_path]",
            VRef::Json(_) => "internal[json]",
            VRef::FinaliseRequest(_) => "internal[finaliser_sentinel]",
            VRef::Catchable(_) => "internal[catchable]",
        }
    }
}

#[derive(Debug)]
pub enum VMut<'a> {
    Null,
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(StringRefMut<'a>),
    Path(&'a mut PathBuf),
    Attrs(&'a mut NixAttrs),
    List(&'a mut NixList),
    Closure(&'a mut Rc<Closure>),
    Builtin(&'a mut Builtin),
    Thunk(&'a mut Thunk),
    AttrNotFound,
    Blueprint(&'a mut Rc<Lambda>),
    DeferredUpvalue(&'a mut StackIdx),
    UnresolvedPath(&'a mut Path),
    Json(&'a mut serde_json::Value),
    FinaliseRequest(&'a mut bool),
    Catchable(&'a mut CatchableErrorKind),
}

impl VMut<'_> {
    pub fn as_ref(&self) -> VRef {
        match self {
            VMut::Null => VRef::Null,
            VMut::Bool(x) => VRef::Bool(*x),
            VMut::Integer(x) => VRef::Integer(*x),
            VMut::Float(x) => VRef::Float(*x),
            VMut::String(x) => VRef::String(x.as_ref()),
            VMut::Path(x) => VRef::Path(x),
            VMut::Attrs(x) => VRef::Attrs(x),
            VMut::List(x) => VRef::List(x),
            VMut::Closure(x) => VRef::Closure(x),
            VMut::Builtin(x) => VRef::Builtin(x),
            VMut::Thunk(x) => VRef::Thunk(x),
            VMut::AttrNotFound => VRef::AttrNotFound,
            VMut::Blueprint(x) => VRef::Blueprint(x),
            VMut::DeferredUpvalue(x) => VRef::DeferredUpvalue(**x),
            VMut::UnresolvedPath(x) => VRef::UnresolvedPath(x),
            VMut::Json(x) => VRef::Json(x),
            VMut::FinaliseRequest(x) => VRef::FinaliseRequest(**x),
            VMut::Catchable(x) => VRef::Catchable(x),
        }
    }
}

impl From<CatchableErrorKind> for Value {
    #[inline]
    fn from(c: CatchableErrorKind) -> Value {
        Value::catchable(c)
    }
}

impl<V> From<Result<V, CatchableErrorKind>> for Value
where
    Value: From<V>,
{
    #[inline]
    fn from(v: Result<V, CatchableErrorKind>) -> Value {
        match v {
            Ok(v) => v.into(),
            Err(e) => Value::catchable(e),
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
            match self.match_ref() {
                $variant => Ok($result),
                VRef::Thunk(thunk) => Self::$name(&thunk.value()),
                other => Err(type_error($expected, other)),
            }
        }
    };
}

/// Generate an `as_*_mut/to_*_mut` accessor method that returns either the
/// expected type, or a type error.
macro_rules! gen_cast_mut {
    ( $name:ident, $type:ty, $expected:expr, $variant:ident) => {
        pub fn $name(&mut self) -> Result<&mut $type, ErrorKind> {
            match self.match_mut() {
                VMut::$variant(x) => Ok(x),
                other => Err(type_error($expected, other.as_ref())),
            }
        }
    };
}

/// Generate an `is_*` type-checking method.
macro_rules! gen_is {
    ( $name:ident, $variant:pat ) => {
        pub fn $name(&self) -> bool {
            match self.match_ref() {
                $variant => true,
                VRef::Thunk(thunk) => Self::$name(&thunk.value()),
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

impl<T> From<T> for Value
where
    T: Into<NixString>,
{
    fn from(t: T) -> Self {
        Self::string(t.into())
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
    pub fn type_of(&self) -> &'static str {
        self.match_ref().type_of()
    }

    /// Deeply forces a value, traversing e.g. lists and attribute sets and forcing
    /// their contents, too.
    ///
    /// This is a generator function.
    pub(super) async fn deep_force(self, co: GenCo, span: LightSpan) -> Result<Value, ErrorKind> {
        if let Some(v) = Self::deep_force_(self.clone(), co, span).await? {
            Ok(v)
        } else {
            Ok(self)
        }
    }

    /// Returns Some(v) or None to indicate the returned value is myself
    async fn deep_force_(
        myself: Value,
        co: GenCo,
        span: LightSpan,
    ) -> Result<Option<Value>, ErrorKind> {
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
            let value = if let VRef::Thunk(t) = v.match_ref() {
                if !thunk_set.insert(t) {
                    continue;
                }
                Thunk::force_(t.clone(), &co, span.clone()).await?
            } else {
                v
            };

            match value.into_match() {
                // Short-circuit on already evaluated values, or fail on internal values.
                V::Null
                | V::Bool(_)
                | V::Integer(_)
                | V::Float(_)
                | V::String(_)
                | V::Path(_)
                | V::Closure(_)
                | V::Builtin(_) => continue,

                V::List(list) => {
                    for val in list.into_iter().rev() {
                        vals.push(val);
                    }
                    continue;
                }

                V::Attrs(attrs) => {
                    for (_, val) in attrs.into_iter().rev() {
                        vals.push(val);
                    }
                    continue;
                }

                V::Thunk(_) => panic!("Tvix bug: force_value() returned a thunk"),

                V::Catchable(c) => return Ok(Some(Value::catchable(c))),

                v @ (V::AttrNotFound
                | V::Blueprint(_)
                | V::DeferredUpvalue(_)
                | V::UnresolvedPath(_)
                | V::Json(_)
                | V::FinaliseRequest(_)) => {
                    panic!("Tvix bug: internal value left on stack: {}", v.type_of())
                }
            }
        }
    }

    pub async fn coerce_to_string(
        self,
        co: GenCo,
        kind: CoercionKind,
        span: LightSpan,
    ) -> Result<Value, ErrorKind> {
        self.coerce_to_string_(&co, kind, span).await
    }

    /// Coerce a `Value` to a string. See `CoercionKind` for a rundown of what
    /// input types are accepted under what circumstances.
    pub async fn coerce_to_string_(
        self,
        co: &GenCo,
        kind: CoercionKind,
        span: LightSpan,
    ) -> Result<Value, ErrorKind> {
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
                v.force(co, span.clone()).await?
            } else {
                return Ok(Value::string(NixString::new_context_from(context, result)));
            };
            let coerced: Result<BString, _> = match (value.into_match(), kind) {
                // coercions that are always done
                (V::String(mut s), _) => {
                    if let Some(ctx) = s.context_mut() {
                        context = context.join(ctx);
                    }
                    Ok((*s).into())
                }

                // TODO(sterni): Think about proper encoding handling here. This needs
                // general consideration anyways, since one current discrepancy between
                // C++ Nix and Tvix is that the former's strings are arbitrary byte
                // sequences without NUL bytes, whereas Tvix only allows valid
                // Unicode. See also b/189.
                (
                    V::Path(p),
                    CoercionKind {
                        import_paths: true, ..
                    },
                ) => {
                    let imported = generators::request_path_import(co, p).await;
                    // When we import a path from the evaluator, we must attach
                    // its original path as its context.
                    context = context.append(NixContextElement::Plain(
                        imported.to_string_lossy().to_string(),
                    ));
                    Ok(imported.into_os_string().into_encoded_bytes().into())
                }
                (
                    V::Path(p),
                    CoercionKind {
                        import_paths: false,
                        ..
                    },
                ) => Ok(p.into_os_string().into_encoded_bytes().into()),

                // Attribute sets can be converted to strings if they either have an
                // `__toString` attribute which holds a function that receives the
                // set itself or an `outPath` attribute which should be a string.
                // `__toString` is preferred.
                (V::Attrs(attrs), kind) => {
                    if let Some(to_string) = attrs.select("__toString") {
                        let callable = to_string.clone().force(co, span.clone()).await?;

                        // Leave the attribute set on the stack as an argument
                        // to the function call.
                        generators::request_stack_push(co, Value::attrs(attrs.clone())).await;

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
                (V::Null, CoercionKind { strong: true, .. })
                | (V::Bool(false), CoercionKind { strong: true, .. }) => Ok("".into()),
                (V::Bool(true), CoercionKind { strong: true, .. }) => Ok("1".into()),

                (V::Integer(i), CoercionKind { strong: true, .. }) => Ok(format!("{i}").into()),
                (V::Float(f), CoercionKind { strong: true, .. }) => {
                    // contrary to normal Display, coercing a float to a string will
                    // result in unconditional 6 decimal places
                    Ok(format!("{:.6}", f).into())
                }

                // Lists are coerced by coercing their elements and interspersing spaces
                (V::List(list), CoercionKind { strong: true, .. }) => {
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

                (V::Thunk(_), _) => panic!("Tvix bug: force returned unforced thunk"),

                (
                    val @ (V::Closure(_)
                    | V::Builtin(_)
                    | V::Null
                    | V::Bool(_)
                    | V::Integer(_)
                    | V::Float(_)
                    | V::List(_)),
                    _,
                ) => Err(ErrorKind::NotCoercibleToString {
                    from: val.type_of(),
                    kind,
                }),

                (V::Catchable(c), _) => return Ok(Value::catchable(c)),

                (V::AttrNotFound, _)
                | (V::Blueprint(_), _)
                | (V::DeferredUpvalue(_), _)
                | (V::UnresolvedPath(_), _)
                | (V::Json(_), _)
                | (V::FinaliseRequest(_), _) => {
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
        other: Value,
        co: GenCo,
        ptr_eq: PointerEquality,
        span: LightSpan,
    ) -> Result<Value, ErrorKind> {
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
        other: Value,
        co: &GenCo,
        ptr_eq: PointerEquality,
        span: LightSpan,
    ) -> Result<Value, ErrorKind> {
        // this is a stack of ((v1,v2),peq) triples to be compared;
        // after each triple is popped off of the stack, v1 is
        // compared to v2 using peq-mode PointerEquality
        let mut vals: Vec<((Value, Value), PointerEquality)> = vec![((self, other), ptr_eq)];

        loop {
            let ((a, b), ptr_eq) = if let Some(abp) = vals.pop() {
                abp
            } else {
                // stack is empty, so comparison has succeeded
                return Ok(Value::bool(true));
            };
            let a = match a.match_ref() {
                VRef::Thunk(thunk) => {
                    // If both values are thunks, and thunk comparisons are allowed by
                    // pointer, do that and move on.
                    if ptr_eq == PointerEquality::AllowAll {
                        if let VRef::Thunk(t1) = b.match_ref() {
                            if t1.ptr_eq(thunk) {
                                continue;
                            }
                        }
                    };

                    Thunk::force_(thunk.clone(), co, span.clone()).await?
                }

                _ => a,
            };

            let b = b.force(co, span.clone()).await?;

            debug_assert!(!matches!(a.match_ref(), VRef::Thunk(_)));
            debug_assert!(!matches!(b.match_ref(), VRef::Thunk(_)));

            let result = match (a.match_ref(), b.match_ref()) {
                // Trivial comparisons
                (VRef::Catchable(c), _) | (_, VRef::Catchable(c)) => {
                    return Ok(Value::catchable(c.clone()))
                }
                (VRef::Null, VRef::Null) => true,
                (VRef::Bool(b1), VRef::Bool(b2)) => b1 == b2,
                (VRef::String(s1), VRef::String(s2)) => s1.string == s2.string,
                (VRef::Path(p1), VRef::Path(p2)) => p1 == p2,

                // Numerical comparisons (they work between float & int)
                (VRef::Integer(i1), VRef::Integer(i2)) => i1 == i2,
                (VRef::Integer(i), VRef::Float(f)) => i as f64 == f,
                (VRef::Float(f1), VRef::Float(f2)) => f1 == f2,
                (VRef::Float(f), VRef::Integer(i)) => i as f64 == f,

                // List comparisons
                (VRef::List(l1), VRef::List(l2)) => {
                    if ptr_eq >= PointerEquality::AllowNested && l1.ptr_eq(l2) {
                        continue;
                    }

                    if l1.len() != l2.len() {
                        return Ok(Value::bool(false));
                    }

                    vals.extend(l1.iter().rev().cloned().zip(l2.iter().rev().cloned()).zip(
                        std::iter::repeat(std::cmp::max(ptr_eq, PointerEquality::AllowNested)),
                    ));
                    continue;
                }

                (_, VRef::List(_)) | (VRef::List(_), _) => return Ok(Value::bool(false)),

                // Attribute set comparisons
                (VRef::Attrs(a1), VRef::Attrs(a2)) => {
                    if ptr_eq >= PointerEquality::AllowNested && a1.ptr_eq(a2) {
                        continue;
                    }

                    // Special-case for derivation comparisons: If both attribute sets
                    // have `type = derivation`, compare them by `outPath`.
                    #[allow(clippy::single_match)] // might need more match arms later
                    match (a1.select("type"), a2.select("type")) {
                        (Some(v1), Some(v2)) => {
                            let s1 = v1.clone().force(co, span.clone()).await?;
                            if s1.is_catchable() {
                                return Ok(s1);
                            }
                            let s2 = v2.clone().force(co, span.clone()).await?;
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

                                    let out1 = out1.clone().force(co, span.clone()).await?;
                                    let out2 = out2.clone().force(co, span.clone()).await?;

                                    if out1.is_catchable() {
                                        return Ok(out1);
                                    }

                                    if out2.is_catchable() {
                                        return Ok(out2);
                                    }

                                    let result =
                                        out1.to_contextful_str()? == out2.to_contextful_str()?;
                                    if !result {
                                        return Ok(Value::bool(false));
                                    } else {
                                        continue;
                                    }
                                }
                            }
                        }
                        _ => {}
                    };

                    if a1.len() != a2.len() {
                        return Ok(Value::bool(false));
                    }

                    // note that it is important to be careful here with the
                    // order we push the keys and values in order to properly
                    // compare attrsets containing `throw` elements.
                    let iter1 = a1.clone().into_iter_sorted().rev();
                    let iter2 = a2.clone().into_iter_sorted().rev();
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

                (VRef::Attrs(_), _) | (_, VRef::Attrs(_)) => return Ok(Value::bool(false)),

                (VRef::Closure(c1), VRef::Closure(c2))
                    if ptr_eq >= PointerEquality::AllowNested =>
                {
                    if Rc::ptr_eq(c1, c2) {
                        continue;
                    } else {
                        return Ok(Value::bool(false));
                    }
                }

                // Everything else is either incomparable (e.g. internal types) or
                // false.
                _ => return Ok(Value::bool(false)),
            };
            if !result {
                return Ok(Value::bool(false));
            }
        }
    }

    gen_cast!(as_bool, bool, "bool", VRef::Bool(b), b);
    gen_cast!(as_int, i64, "int", VRef::Integer(x), x);
    gen_cast!(as_float, f64, "float", VRef::Float(x), x);

    /// Cast the current value into a **context-less** string.
    /// If you wanted to cast it into a potentially contextful string,
    /// you have to explicitly use `to_contextful_str`.
    /// Contextful strings are special, they should not be obtained
    /// everytime you want a string.
    pub fn to_str(&self) -> Result<NixString, ErrorKind> {
        match self.match_ref() {
            VRef::String(s) if s.context.is_none() => Ok(s.to_owned()),
            VRef::Thunk(thunk) => Self::to_str(&thunk.value()),
            other => Err(type_error("contextless strings", other)),
        }
    }

    gen_cast!(
        to_contextful_str,
        NixString,
        "contextful string",
        VRef::String(s),
        s.to_owned()
    );
    gen_cast!(to_path, PathBuf, "path", VRef::Path(p), p.clone());
    gen_cast!(to_attrs, NixAttrs, "set", VRef::Attrs(a), a.clone());
    gen_cast!(to_list, NixList, "list", VRef::List(l), l.clone());
    gen_cast!(
        as_closure,
        Rc<Closure>,
        "lambda",
        VRef::Closure(c),
        c.clone()
    );

    gen_cast_mut!(as_list_mut, NixList, "list", List);

    pub fn is_integer(&self) -> bool {
        self.0.is_int()
    }

    pub fn is_float(&self) -> bool {
        self.0.is_float()
    }

    pub fn is_number(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    gen_is!(is_path, VRef::Path(_));
    gen_is!(is_bool, VRef::Bool(_));
    gen_is!(is_attrs, VRef::Attrs(_));
    gen_is!(is_catchable, VRef::Catchable(_));

    /// Returns `true` if the value is a [`Thunk`].
    ///
    /// [`Thunk`]: Value::Thunk
    pub fn is_thunk(&self) -> bool {
        matches!(self.match_ref(), VRef::Thunk(..))
    }

    /// Compare `self` against other using (fallible) Nix ordering semantics.
    ///
    /// The function is intended to be used from within other generator
    /// functions or `gen!` blocks.
    pub async fn nix_cmp_ordering(
        self,
        other: Self,
        co: GenCo,
        span: LightSpan,
    ) -> Result<Result<Ordering, CatchableErrorKind>, ErrorKind> {
        Self::nix_cmp_ordering_(self, other, co, span).await
    }

    async fn nix_cmp_ordering_(
        myself: Self,
        other: Self,
        co: GenCo,
        span: LightSpan,
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
                    .nix_eq(b.clone(), &co, PointerEquality::AllowAll, span.clone())
                    .await?
                    .as_bool()?
                {
                    continue;
                }
                a = a.force(&co, span.clone()).await?;
                b = b.force(&co, span.clone()).await?;
            }
            let result = match (a.into_match(), b.into_match()) {
                (V::Catchable(c), _) => return Ok(Err(c)),
                (_, V::Catchable(c)) => return Ok(Err(c)),
                // same types
                (V::Integer(i1), V::Integer(i2)) => i1.cmp(&i2),
                (V::Float(f1), V::Float(f2)) => f1.total_cmp(&f2),
                (V::String(s1), V::String(s2)) => s1.cmp(&s2),
                (V::List(l1), V::List(l2)) => {
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
                (V::Integer(i1), V::Float(f2)) => (i1 as f64).total_cmp(&f2),
                (V::Float(f1), V::Integer(i2)) => f1.total_cmp(&(i2 as f64)),

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
    pub async fn force(self, co: &GenCo, span: LightSpan) -> Result<Value, ErrorKind> {
        match self.into_thunk() {
            // TODO(amjoseph): use #[tailcall::mutual]
            Ok(thunk) => Thunk::force_(thunk, co, span).await,
            Err(v) => Ok(v),
        }
    }

    // need two flavors, because async
    pub async fn force_owned_genco(self, co: GenCo, span: LightSpan) -> Result<Value, ErrorKind> {
        match self.into_thunk() {
            // TODO(amjoseph): use #[tailcall::mutual]
            Ok(thunk) => Thunk::force_(thunk, &co, span).await,
            Err(v) => Ok(v),
        }
    }

    /// Explain a value in a human-readable way, e.g. by presenting
    /// the docstrings of functions if present.
    pub fn explain(&self) -> String {
        match self.match_ref() {
            VRef::Null => "the 'null' value".into(),
            VRef::Bool(b) => format!("the boolean value '{}'", b),
            VRef::Integer(i) => format!("the integer '{}'", i),
            VRef::Float(f) => format!("the float '{}'", f),
            VRef::String(s) if s.has_context() => {
                format!("the contextful string '{}'", s.to_owned())
            }
            VRef::String(s) => format!("the contextless string '{}'", s.to_owned()),
            VRef::Path(p) => format!("the path '{}'", p.to_string_lossy()),
            VRef::Attrs(attrs) => format!("a {}-item attribute set", attrs.len()),
            VRef::List(list) => format!("a {}-item list", list.len()),

            VRef::Closure(f) => {
                if let Some(name) = &f.lambda.name {
                    format!("the user-defined Nix function '{}'", name)
                } else {
                    "a user-defined Nix function".to_string()
                }
            }

            VRef::Builtin(b) => {
                let mut out = format!("the builtin function '{}'", b.name());
                if let Some(docs) = b.documentation() {
                    out.push_str("\n\n");
                    out.push_str(docs);
                }
                out
            }

            // TODO: handle suspended thunks with a different explanation instead of panicking
            VRef::Thunk(t) => t.value().explain(),

            VRef::Catchable(_) => "a catchable failure".into(),

            VRef::AttrNotFound
            | VRef::Blueprint(_)
            | VRef::DeferredUpvalue(_)
            | VRef::UnresolvedPath(_)
            | VRef::Json(_)
            | VRef::FinaliseRequest(_) => "an internal Tvix evaluator value".into(),
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
    // a digit, add the missing `+` sign.
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
            s = &mut new_s
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
                        new_s.extend_from_slice(&s[0..=i - 1]);
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
        match self.match_ref() {
            VRef::Null => f.write_str("null"),
            VRef::Bool(true) => f.write_str("true"),
            VRef::Bool(false) => f.write_str("false"),
            VRef::Integer(num) => write!(f, "{}", num),
            VRef::String(s) => write!(f, "{}", s.to_owned()),
            VRef::Path(p) => write!(f, "{}", p.display()),
            VRef::Attrs(attrs) => attrs.total_fmt(f, set),
            VRef::List(list) => list.total_fmt(f, set),
            // TODO: fancy REPL display with position
            VRef::Closure(_) => f.write_str("<LAMBDA>"),
            VRef::Builtin(builtin) => write!(f, "{}", builtin),

            // Nix prints floats with a maximum precision of 5 digits
            // only. Except when it decides to use scientific notation
            // (with a + after the `e`, and zero-padded to 0 digits)
            VRef::Float(num) => total_fmt_float(num, f),

            // internal types
            VRef::AttrNotFound => f.write_str("internal[not found]"),
            VRef::Blueprint(_) => f.write_str("internal[blueprint]"),
            VRef::DeferredUpvalue(_) => f.write_str("internal[deferred_upvalue]"),
            VRef::UnresolvedPath(_) => f.write_str("internal[unresolved_path]"),
            VRef::Json(_) => f.write_str("internal[json]"),
            VRef::FinaliseRequest(_) => f.write_str("internal[finaliser_sentinel]"),

            // Delegate thunk display to the type, as it must handle
            // the case of already evaluated or cyclic thunks.
            VRef::Thunk(t) => t.total_fmt(f, set),
            VRef::Catchable(_) => panic!("total_fmt() called on a CatchableErrorKind"),
        }
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Self::bool(b)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Self::integer(i)
    }
}

impl From<f64> for Value {
    fn from(i: f64) -> Self {
        Self::float(i)
    }
}

impl From<PathBuf> for Value {
    fn from(path: PathBuf) -> Self {
        Self::object(Object::Path(path))
    }
}

impl From<V> for Value {
    fn from(v: V) -> Self {
        match v {
            V::Null => Value::NULL,
            V::Bool(x) => Value::bool(x),
            V::Integer(x) => Value::integer(x),
            V::Float(x) => Value::float(x),
            V::String(x) => Value::string(x),
            V::Path(x) => Value::path(x),
            V::Attrs(x) => Value::attrs(x),
            V::List(x) => Value::list(x),
            V::Closure(x) => Value::closure(x),
            V::Builtin(x) => Value::builtin(x),
            V::Thunk(x) => Value::thunk(x),
            V::AttrNotFound => Value::attr_not_found(),
            V::Blueprint(x) => Value::blueprint(x),
            V::DeferredUpvalue(x) => Value::deferred_upvalue(x),
            V::UnresolvedPath(x) => Value::unresolved_path(x),
            V::Json(x) => Value::json(x),
            V::FinaliseRequest(x) => Value::finalise_request(x),
            V::Catchable(x) => Value::catchable(x),
        }
    }
}

fn type_error(expected: &'static str, actual: VRef) -> ErrorKind {
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
        assert_eq!(size_of::<Value>(), 8);
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
            for (n, expected) in ff.iter() {
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
