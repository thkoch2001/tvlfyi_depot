//! syn is an *abstractly* abstract syntax tree for Nix.  That isn't a typo.
//!
//! This module provides traits which abstract away everything the compiler
//! might want to know about the parsed Nix syntax tree.  It abstracts over
//! the implementation of abstract syntax trees.  Yo dawg, I heard you like...
//!
//! By abstracting away only the types and methods that the compiler needs, we
//! can easily swap out the representation.  For example, instead of rnix's
//! (misleadingly-named) concrete syntax tree `rnix::ast` (which should be
//! named `rnix::cst`) we can switch to a true abstract syntax tree.  This
//! would allow to implement parts of the compiler as AST-to-AST
//! transformations, making them completely independent from the rest of the
//! compiler, which would then be much simpler.  Since `syn` completely hides
//! the underlying implementation this change can be made without touching the
//! rest of the compiler.  In fact, arbitrarily many AST-to-AST passes can be
//! added this way.  The GHC renamer provides many examples of useful passes.
//!
//! The contents of this file, including its public declarations, are
//! considered an implementation detail of tvix::eval::compiler.  They are not
//! visible anywhere outside of tvix::eval::compiler and probably never will
//! be.

use super::*;
pub mod cst;

/// Implementations of the `Syntax` trait exist so that things outside this
/// module can have one single polymorphic paramter <Syntax> instead of one
/// for every different flavor of ast-node (<AttrPath, Entry, PhamNuwen, ...>)
///
/// The structs which implement this trait are typically zero-sized types.
///
/// Each of the associated types of Syntax implements a trait (with the same
/// name -- it's a pun).  Each trait is generic over <Syntax>; you can think
/// of this generic as the "back pointer" from each element of Syntax to the
/// containing collection.  These "back pointers" are generics rather than
/// associated types in order to avoid Rust's clumsy syntax -- if you use
/// associated types you end up having to write long, hard-to-read `<<<X as Y>
/// as Z> as Q>` chains.
pub trait Syntax: 'static {
    type Ident: Ident<Self>;
    type AttrName: AttrName<Self>;
    type AttrPath: AttrPath<Self>;
    type AttrPathValue: AttrPathValue<Self>;
    type AttrSet: AttrSet<Self>;
    type Entry: Entry<Self>;
    type Inherit: Inherit<Self>;
    type LetIn: LetIn<Self>;
    type LegacyLet: LegacyLet<Self>;
    type Expr: Expr<Self>;
}

/// Types which implement this trait represent identifier expressions.
pub trait Ident<Syntax>: ToSpan + Clone + FromCst<rnix::ast::Ident>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    /// The identifier, as a Rust string.
    fn ident(&self) -> &str;

    /// Creates a new identifier from a Rust string and a source code Span.
    fn new(str: String, span: Span) -> Self;
}

/// Types which implement this trait represent attribute names, which are
/// either an identifier (static, e.g. `fred` or `bob."0123"`) or an
/// expression (dynamic, eg `bob.${carl}`)
pub trait AttrName<Syntax>:
    FromCst<rnix::ast::Attr>
    + ToSpan
    + Clone
    + From<<Syntax as self::Syntax>::Ident>
    + From<<Syntax as self::Syntax>::Expr>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    /// Return Some(&self) if this is a static attribute name, like both of
    /// the attrnames in `inherit "0123" fred;`
    fn as_static(&self) -> Option<&<Syntax as self::Syntax>::Ident>;

    /// Return Some(&self) if this is a dynamic attribute name, like `{
    /// ${carl} = ...; }`
    fn as_dynamic(&self) -> Option<&<Syntax as self::Syntax>::Expr>;
}

/// Types which implement this trait represent attribute paths, like
/// `stdenv.cc.cc` and `bob."${fred}".carl`.  Both examples have three
/// components.
///
/// Although Nix attrpaths always have at least one component, this trait
/// allows for "zero-length" attribute paths in order to play nicely with
/// Rust's std::iter traits -- "making the impossible unrepresentable" here
/// would mean we wouldn't be able to use FromIterator and IntoIterator.
pub trait AttrPath<Syntax>:
    Clone
    + FromCst<rnix::ast::Attrpath>
    + IntoIterator<Item = <Syntax as self::Syntax>::AttrName>
    + FromIterator<<Syntax as self::Syntax>::AttrName>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    /// Returns the first element of the attribute path, if any.
    fn first(&self) -> Option<&<Syntax as self::Syntax>::AttrName>;
}

/// Types which implement this trait represent an attribute path and an
/// expression for the value assigned to it.  For example, `meta.license =
/// lib.licenses.gpl3`.
pub trait AttrPathValue<Syntax>:
    Clone + ToSpan + FromCst<rnix::ast::AttrpathValue> + Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    /// Returns the attrpath -- the portion before the equals sign.
    fn path(&self) -> Syntax::AttrPath;

    /// Returns the value -- the portion after the equals sign.
    fn value(&self) -> Syntax::Expr;

    /// Creates a new AttrPathValue out of an AttrPath and an expression
    fn new(span: Span, path: Syntax::AttrPath, value: Syntax::Expr) -> Self;
}

/// Types which implement this trait represent attribute set literals.
pub trait AttrSet<Syntax>:
    ToSpan + Clone + FromCst<rnix::ast::AttrSet> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    /// True if this attribute set literal is recursive, i.e. `rec { ... }`
    fn is_rec(&self) -> bool;

    /// This method will be removed once the rest of eval::compiler is
    /// converted to use syn::Syntax.  Please don't add additional uses of it.
    fn to_cst(self) -> rnix::ast::AttrSet;
}

/// Types which implement this trait can be entries in a collection of
/// bindings -- either `let .. in` or attrset literals.  This is the least
/// common supertrait of Inherit and AttrPathValue.
pub trait Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
}

/// Types which implement this trait represent `inherit` clauses, both
/// namespaced and non-namespaced.
pub trait Inherit<Syntax>: Clone + ToSpan + FromCst<rnix::ast::Inherit> + Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    fn namespace(&self) -> Option<Syntax::Expr>;
    fn attrs<'a>(&'a self) -> impl Iterator<Item = &'a <Syntax as self::Syntax>::Ident>
    where
        Syntax: 'static;
    fn new(
        namespace: Option<Syntax::Expr>,
        attrs: impl Iterator<Item = <Syntax as self::Syntax>::Ident>,
        span: Span,
    ) -> Self;
}

/// Types which implement this trait represent `let..in` expressions.
pub trait LetIn<Syntax>: ToSpan + Clone + FromCst<rnix::ast::LetIn> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    fn body(self) -> Syntax::Expr;
}

pub trait LegacyLet<Syntax>:
    ToSpan + Clone + FromCst<rnix::ast::LegacyLet> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
}

/// Types which implement this trait represent Nix expressions.  This trait
/// will accrue more methods as the rest of the compiler is migrated to
/// syn::Syntax.
pub trait Expr<Syntax>: ToSpan + FromCst<ast::Expr> + Clone
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    fn to_cst(self) -> rnix::ast::Expr;
    fn as_cst(&self) -> &rnix::ast::Expr;
    fn is_attrset(&self) -> bool;
    fn as_attrset(self, c: &mut Compiler) -> Option<Syntax::AttrSet>;
}

/// Trait for things which have an attribute entry associated with them.  An
/// attribute entry is either an `Inherit` or an `AttrPathValue`.
pub trait HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized + 'static,
{
    fn inherits(&self, c: &mut Compiler) -> impl Iterator<Item = Syntax::Inherit>;
    fn attributes<'a>(&self, c: &'a mut Compiler) -> impl Iterator<Item = Syntax::AttrPathValue>;
}

/// Trait for things that can be created from an rnix Concrete Syntax Tree of
/// type `CstType`.
pub trait FromCst<CstType> {
    // `Compiler` is `&mut` because we may need to report errors
    fn from_cst(c: &mut Compiler, cst: CstType) -> Self
    where
        Self: Sized;
}
