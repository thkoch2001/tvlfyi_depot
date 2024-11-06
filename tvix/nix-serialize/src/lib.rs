//! # Nix Serialize
//!
//! This crate provides (de)serialization machinery for the nix-daemon protocol.
//!
//! It's main API surface is [`NixDeserialize`] and [`NixSerialize`] traits,
//! as well as concrete writer/reader implementations: [`NixWriter`], [`NixReader`].
//!
//! [`NixDeserialize`]: crate::NixDeserialize
//! [`NixSerialize`]: crate::NixSerialize
//! [`NixWriter`]: crate::NixWriter
//! [`NixReader`]: crate::NixReader
//!
#[cfg_attr(
    feature = "derive",
    doc = r##"
# Using derive
1. [Overview](#overview)
3. [Attributes](#attributes)
    1. [Container attributes](#container-attributes)
        1. [`#[nix(from_str)]`](#nixfrom_str)
        2. [`#[nix(from = "FromType")]`](#nixfrom--fromtype)
        3. [`#[nix(try_from = "FromType")]`](#nixtry_from--fromtype)
        4. [`#[nix(into = "IntoType")]`](#nixinto--intotype)
        5. [`#[nix(try_into = "IntoType")]`](#nixtry_into--intotype)
        6. [`#[nix(display)]`](#nixdisplay)
        7. [`#[nix(display = "path")]`](#nixdisplay--path)
        8. [`#[nix(crate = "...")]`](#nixcrate--)
    2. [Variant attributes](#variant-attributes)
        1. [`#[nix(version = "range")]`](#nixversion--range)
    3. [Field attributes](#field-attributes)
        1. [`#[nix(version = "range")]`](#nixversion--range-1)
        2. [`#[nix(default)]`](#nixdefault)
        3. [`#[nix(default = "path")]`](#nixdefault--path)
## Overview
This crate contains derive macros and function-like macros for implementing
`NixDeserialize` and `NixSerialize` with less boilerplate.
### Examples
```rust
# use nix_serialize::{NixDeserialize, NixSerialize};
#
#[derive(NixDeserialize, NixSerialize)]
struct Unnamed(u64, String);
```
```rust
# use nix_serialize::{NixDeserialize, NixSerialize};
#
#[derive(NixDeserialize, NixSerialize)]
struct Fields {
    number: u64,
    message: String,
};
```
```rust
# use nix_serialize::{NixDeserialize, NixSerialize};
#
#[derive(NixDeserialize, NixSerialize)]
struct Ignored;
```
## Attributes
To customize the derived trait implementations you can add
[attributes](https://doc.rust-lang.org/reference/attributes.html)
to containers, fields and variants.
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
#[nix(crate="nix_serialize")] // <-- This is a container attribute
struct Fields {
    number: u64,
    #[nix(version="..20")] // <-- This is a field attribute
    message: String,
};
#[derive(NixDeserialize)]
#[nix(crate="nix_serialize")] // <-- This is also a container attribute
enum E {
    #[nix(version="..10")] // <-- This is a variant attribute
    A(u64),
    #[nix(version="10..")] // <-- This is also a variant attribute
    B(String),
}
```
### Container attributes
##### `#[nix(from_str)]`
When `from_str` is specified the fields are all ignored and instead a
`String` is first deserialized and then `FromStr::from_str` is used
to convert this `String` to the container type.
This means that the container must implement `FromStr` and the error
returned from the `from_str` must implement `Display`.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
#[nix(from_str)]
struct MyString(String);
impl std::str::FromStr for MyString {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s != "bad string" {
            Ok(MyString(s.to_string()))
        } else {
            Err("Got a bad string".to_string())
        }
    }
}
```
##### `#[nix(from = "FromType")]`
When `from` is specified the fields are all ignored and instead a
value of `FromType` is first deserialized and then `From::from` is
used to convert from this value to the container type.
This means that the container must implement `From<FromType>` and
`FromType` must implement `NixDeserialize`.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
#[nix(from="usize")]
struct MyValue(usize);
impl From<usize> for MyValue {
    fn from(val: usize) -> Self {
        MyValue(val)
    }
}
```
##### `#[nix(try_from = "FromType")]`
With `try_from` a value of `FromType` is first deserialized and then
`TryFrom::try_from` is used to convert from this value to the container
type.
This means that the container must implement `TryFrom<FromType>` and
`FromType` must implement `NixDeserialize`.
The error returned from `try_from` also needs to implement `Display`.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
#[nix(try_from="usize")]
struct WrongAnswer(usize);
impl TryFrom<usize> for WrongAnswer {
    type Error = String;
    fn try_from(val: usize) -> Result<Self, Self::Error> {
        if val != 42 {
            Ok(WrongAnswer(val))
        } else {
            Err("Got the answer to life the universe and everything".to_string())
        }
    }
}
```
##### `#[nix(into = "IntoType")]`
When `into` is specified the fields are all ignored and instead the
container type is converted to `IntoType` using `Into::into` and
`IntoType` is then serialized. Before converting `Clone::clone` is
called.
This means that the container must implement `Into<IntoType>` and `Clone`
and `IntoType` must implement `NixSerialize`.
###### Example
```rust
# use nix_serialize::NixSerialize;
#
#[derive(Clone, NixSerialize)]
#[nix(into="usize")]
struct MyValue(usize);
impl From<MyValue> for usize {
    fn from(val: MyValue) -> Self {
        val.0
    }
}
```
##### `#[nix(try_into = "IntoType")]`
When `try_into` is specified the fields are all ignored and instead the
container type is converted to `IntoType` using `TryInto::try_into` and
`IntoType` is then serialized. Before converting `Clone::clone` is
called.
This means that the container must implement `TryInto<IntoType>` and
`Clone` and `IntoType` must implement `NixSerialize`.
The error returned from `try_into` also needs to implement `Display`.
###### Example
```rust
# use nix_serialize::NixSerialize;
#
#[derive(Clone, NixSerialize)]
#[nix(try_into="usize")]
struct WrongAnswer(usize);
impl TryFrom<WrongAnswer> for usize {
    type Error = String;
    fn try_from(val: WrongAnswer) -> Result<Self, Self::Error> {
        if val.0 != 42 {
            Ok(val.0)
        } else {
            Err("Got the answer to life the universe and everything".to_string())
        }
    }
}
```
##### `#[nix(display)]`
When `display` is specified the fields are all ignored and instead the
container must implement `Display` and `NixWrite::write_display` is used to
write the container.
###### Example
```rust
# use nix_serialize::NixSerialize;
# use std::fmt::{Display, Result, Formatter};
#
#[derive(NixSerialize)]
#[nix(display)]
struct WrongAnswer(usize);
impl Display for WrongAnswer {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Wrong Answer = {}", self.0)
    }
}
```
##### `#[nix(display = "path")]`
When `display` is specified the fields are all ignored and instead the
container the specified path must point to a function that is callable as
`fn(&T) -> impl Display`. The result from this call is then written with
`NixWrite::write_display`.
For example `default = "my_value"` would call `my_value(&self)` and `display =
"AType::empty"` would call `AType::empty(&self)`.
###### Example
```rust
# use nix_serialize::NixSerialize;
# use std::fmt::{Display, Result, Formatter};
#
#[derive(NixSerialize)]
#[nix(display = "format_it")]
struct WrongAnswer(usize);
struct WrongDisplay<'a>(&'a WrongAnswer);
impl<'a> Display for WrongDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Wrong Answer = {}", self.0.0)
    }
}
fn format_it(value: &WrongAnswer) -> impl Display + '_ {
    WrongDisplay(value)
}
```
##### `#[nix(crate = "...")]`
Specify the path to the `nix-compat` crate instance to use when referring
to the API in the generated code. This is usually not needed.
### Variant attributes
##### `#[nix(version = "range")]`
Specifies the protocol version range where this variant is used.
When deriving an enum the `version` attribute is used to select which
variant of the enum to deserialize. The range is for minor version and
the version ranges of all variants combined must cover all versions
without any overlap or the first variant that matches is selected.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
enum Testing {
    #[nix(version="..=18")]
    OldVersion(u64),
    #[nix(version="19..")]
    NewVersion(String),
}
```
### Field attributes
##### `#[nix(version = "range")]`
Specifies the protocol version range where this field is included.
The range is for minor version. For example `version = "..20"`
includes the field in protocol versions `1.0` to `1.19` and skips
it in version `1.20` and above.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
struct Field {
    number: u64,
    #[nix(version="..20")]
    messsage: String,
}
```
##### `#[nix(default)]`
When a field is skipped because the active protocol version falls
outside the range specified in [`#[nix(version = "range")]`](#nixversion--range-1)
this attribute indicates that `Default::default()` should be used
to get a value for the field. This is also the default
when you only specify [`#[nix(version = "range")]`](#nixversion--range-1).
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
struct Field {
    number: u64,
    #[nix(version="..20", default)]
    messsage: String,
}
```
##### `#[nix(default = "path")]`
When a field is skipped because the active protocol version falls
outside the range specified in [`#[nix(version = "range")]`](#nixversion--range-1)
this attribute indicates that the function in `path` should be called to
get a default value for the field. The given function must be callable
as `fn() -> T`.
For example `default = "my_value"` would call `my_value()` and `default =
"AType::empty"` would call `AType::empty()`.
###### Example
```rust
# use nix_serialize::NixDeserialize;
#
#[derive(NixDeserialize)]
struct Field {
    number: u64,
    #[nix(version="..20", default="missing_string")]
    messsage: String,
}
fn missing_string() -> String {
    "missing string".to_string()
}
```

## Deriving NixDeserialize for types you don't own

Sometimes you can't use the deriver to implement `NixDeserialize`/`NixSerialize`
(like when dealing with types in Rust standard library) but don't want
to implement it yourself. So this macro can be used for those situations
where you would derive using `#[nix(from_str)]`,
`#[nix(from = "FromType")]` or `#[nix(try_from = "FromType")]` if you
could.
#### Example
```rust
# use nix_serialize::nix_deserialize_remote;
#
struct MyU64(u64);
impl From<u64> for MyU64 {
    fn from(value: u64) -> Self {
        Self(value)
    }
}
nix_deserialize_remote!(#[nix(from="u64")] MyU64);
```

## Deriving NixSerialize for types you don't own

Sometimes you can't use the deriver to implement `NixSerialize`
(like when dealing with types in Rust standard library) but don't want
to implement it yourself. So this macro can be used for those situations
where you would derive using `#[nix(display)]`, `#[nix(display = "path")]`,
`#[nix(store_dir_display)]`, `#[nix(into = "IntoType")]` or
`#[nix(try_into = "IntoType")]` if you could.
#### Example
```rust
# use nix_serialize::nix_serialize_remote;
#
#[derive(Clone)]
struct MyU64(u64);
impl From<MyU64> for u64 {
    fn from(value: MyU64) -> Self {
        value.0
    }
}
nix_serialize_remote!(#[nix(into="u64")] MyU64);
```
"##
)]
mod de;
mod protocol_version;
mod ser;

pub use de::{Error as DeserializeError, NixDeserialize, NixRead, NixReader, NixReaderBuilder};
pub use protocol_version::ProtocolVersion;
pub use ser::{Error as SerializeError, NixSerialize, NixWrite, NixWriter, NixWriterBuilder};

#[cfg(any(test, feature = "test"))]
pub use de::mock as de_mock;

#[cfg(any(test, feature = "test"))]
pub use ser::mock as ser_mock;

#[cfg(feature = "nix-serialize-derive")]
extern crate nix_serialize_derive;

#[cfg(feature = "nix-serialize-derive")]
#[cfg_attr(docsrs, doc(cfg(feature = "derive")))]
pub use nix_serialize_derive::{
    nix_deserialize_remote, nix_serialize_remote, NixDeserialize, NixSerialize,
};

/// 8 null bytes, used to write out padding.
pub(crate) const EMPTY_BYTES: &[u8; 8] = &[0u8; 8];
