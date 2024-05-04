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
pub trait Syntax {
    type Ident: Ident<Self>;
    type Inherit: Inherit<Self>;
    type AttrPath: AttrPath<Self>;
    type AttrPathValue: AttrPathValue<Self>;
    type AttrSet: AttrSet<Self>;
    type Entry: Entry<Self>;
    type LetIn: LetIn<Self>;
    type LegacyLet: LegacyLet<Self>;
    type Expr: Expr<Self>;
}

pub enum AttrName<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    Static(<Syntax as self::Syntax>::Ident),
    Dynamic(<Syntax as self::Syntax>::Expr),
}

impl<Syntax> Clone for AttrName<Syntax>
where
    Syntax: self::Syntax,
{
    fn clone(&self) -> Self {
        match self {
            AttrName::Static(attrname_static) => AttrName::Static(attrname_static.clone()),
            AttrName::Dynamic(attrname_dynamic) => AttrName::Dynamic(attrname_dynamic.clone()),
        }
    }
}

impl<Syntax> FromCst<rnix::ast::Attr> for AttrName<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn from_cst(c: &Compiler, cst: rnix::ast::Attr) -> Self {
        let span = c.span_for(&cst);
        if let Some(s) = expr_static_attr_str(&cst) {
            AttrName::Static(<Syntax as self::Syntax>::Ident::new(s.into(), span))
        } else {
            let expr = match cst {
                ast::Attr::Dynamic(expr) => expr.expr().unwrap(),
                ast::Attr::Str(str) => rnix::ast::Expr::Str(str),
                ast::Attr::Ident(_expr) => {
                    unreachable!(
                        "if you see this, it means that expr_static_attr_str() has a bug in it"
                    )
                }
            };
            AttrName::Dynamic(<Syntax as self::Syntax>::Expr::from_cst(c, expr))
        }
    }
}

impl<Syntax> ToSpan for AttrName<Syntax>
where
    Syntax: syn::Syntax + ?Sized,
{
    fn span_for(&self, file: &codemap::File) -> Span {
        match self {
            AttrName::Static(attrname_static) => attrname_static.span_for(file),
            AttrName::Dynamic(attrname_dynamic) => attrname_dynamic.span_for(file),
        }
    }
}

/// Least common supertrait of `Static` and `Dynamic`
impl<Syntax> AttrName<Syntax>
where
    Syntax: self::Syntax,
{
    pub fn as_static(&self) -> Option<&<Syntax as self::Syntax>::Ident> {
        match self {
            AttrName::Static(inner) => Some(inner),
            _ => None,
        }
    }
}

/// Types which implement this trait represent attribute paths, like
/// `stdenv.cc.cc` and `bob."${fred}".carl`.  Both examples have three
/// components.
pub trait AttrPath<Syntax>: Clone + FromCst<rnix::ast::Attrpath>
where
    Syntax: self::Syntax + ?Sized,
{
    fn first(&mut self) -> Option<&AttrName<Syntax>>;
    fn take_first(&mut self) -> Option<AttrName<Syntax>>;
}

/// Types which implement this trait represent an attribute path and an
/// expression for the value assigned to it.  For example, `meta.license =
/// lib.licenses.gpl3`.
pub trait AttrPathValue<Syntax>:
    Clone + ToSpan + FromCst<rnix::ast::AttrpathValue> + Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn path(&self) -> Syntax::AttrPath;
    fn value(&self) -> Syntax::Expr;
    fn new(span: Span, path: Syntax::AttrPath, value: Syntax::Expr) -> Self;
}

/// Types which implement this trait represent `inherit` clauses, both
/// namespaced and non-namespaced.
pub trait Inherit<Syntax>: Clone + ToSpan + FromCst<rnix::ast::Inherit> + Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn namespace(&self) -> Option<Syntax::Expr>;
    fn attrs(&self) -> Vec<AttrName<Syntax>>;
    fn new(namespace: Option<Syntax::Expr>, attrs: Vec<AttrName<Syntax>>, span: Span) -> Self;
}

/// Types which implement this trait can be entries in a collection of
/// bindings -- either `let .. in` or attrset literals.  This is the least
/// common supertrait of Inherit and AttrPathValue.
pub trait Entry<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
}

/// Types which implement this trait represent `let..in` expressions.
pub trait LetIn<Syntax>: ToSpan + Clone + FromCst<rnix::ast::LetIn> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn body(self) -> Syntax::Expr;
}

/// Types which implement this trait represent attribute set literals.
pub trait AttrSet<Syntax>:
    ToSpan + Clone + FromCst<rnix::ast::AttrSet> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn is_rec(&self) -> bool;
    fn to_cst(self) -> rnix::ast::AttrSet;
}

pub trait LegacyLet<Syntax>:
    ToSpan + Clone + FromCst<rnix::ast::LegacyLet> + HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
}

/// Types which implement this trait represent identifier expressions.
pub trait Ident<Syntax>: ToSpan + Clone + FromCst<rnix::ast::Ident>
where
    Syntax: self::Syntax + ?Sized,
{
    fn ident(&self) -> &str;
    fn new(str: String, span: Span) -> Self;
}

/// Types which implement this trait represent Nix expressions.  This trait
/// will accrue more methods as the rest of the compiler is migrated to
/// syn::Syntax.
pub trait Expr<Syntax>: ToSpan + FromCst<ast::Expr> + Clone
where
    Syntax: self::Syntax + ?Sized,
{
    fn to_cst(self) -> rnix::ast::Expr;
    fn as_cst(&self) -> &rnix::ast::Expr;
    fn is_attrset(&self) -> bool;
    fn as_attrset(self, c: &Compiler) -> Option<Syntax::AttrSet>;
}

/// Trait for things which have an attribute entry associated with them.  An
/// attribute entry is either an `Inherit` or an `AttrPathValue`.
pub trait HasEntries<Syntax>
where
    Syntax: self::Syntax + ?Sized,
{
    fn inherits(&self, c: &Compiler) -> Vec<Syntax::Inherit>;
    fn attributes<'a>(&self, c: &Compiler) -> Vec<Syntax::AttrPathValue>;
}

/// Trait for things that can be created from an rnix Concrete Syntax Tree of
/// type `CstType`.
pub trait FromCst<CstType> {
    fn from_cst(c: &Compiler, cst: CstType) -> Self;
}
