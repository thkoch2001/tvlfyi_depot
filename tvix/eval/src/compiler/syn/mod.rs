//! syn is an *abstractly* abstract syntax tree for Nix.  That isn't a typo.
//!
//! This module provides traits which abstract away everything the compiler
//! might want to know about the parsed Nix syntax tree.  It abstracts over
//! the implementation of abstract syntax trees.  Yo dawg.
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

/// Implementations of the trait Syntax exist so that things outside this
/// module can have one single polymorphic paramter <Syntax> instead of one
/// for every different flavor of ast-node (<AttrPath, Entry, PhamNuwen, ...>)
///
/// The structs which implement this trait are typically zero-sized types.
///
pub trait Syntax {
    type Attr: Attr<Expr = Self::Expr>;
    type AttrPath: AttrPath<Attr = Self::Attr>;
    type AttrPathValue: AttrPathValue<AttrPath = Self::AttrPath, Expr = Self::Expr>;
    type Inherit: Inherit<Expr = Self::Expr, Attr = Self::Attr>;
    type Expr: Expr;
}

/// It's 2024 and there is still no std::either.  Wtf.
#[derive(Clone)]
pub enum StaticOrDynamic<Static, Dynamic> {
    Static(Static),
    Dynamic(Dynamic),
}

/// Trait expressing that its implementor can be created from an rnix Concrete
/// Syntax Tree of type `CstType`.
pub trait FromCst<CstType> {
    fn from_cst(c: &Compiler, cst: CstType) -> Self;
}

pub trait Attr: Clone + ToSpan + FromCst<rnix::ast::Attr> {
    type Expr: self::Expr;
    fn attr(&self) -> &StaticOrDynamic<SmolStr, Self::Expr>;
    fn into_attr(self) -> StaticOrDynamic<SmolStr, Self::Expr>;
    fn from_static(str: SmolStr, span: Span) -> Self;
    fn from_dynamic(expr: Self::Expr, span: Span) -> Self;
    fn as_static(&self) -> Option<SmolStr> {
        match self.attr() {
            StaticOrDynamic::Static(smol_str) => Some(smol_str.clone()),
            StaticOrDynamic::Dynamic(_) => None,
        }
    }
    fn as_dynamic(self) -> Option<Self::Expr> {
        match self.into_attr() {
            StaticOrDynamic::Static(_) => None,
            StaticOrDynamic::Dynamic(expr) => Some(expr),
        }
    }
    fn is_static(&self) -> bool {
        match self.attr() {
            StaticOrDynamic::Static(_) => true,
            StaticOrDynamic::Dynamic(_) => false,
        }
    }
    fn is_dynamic(&self) -> bool {
        !self.is_static()
    }
}

pub trait Inherit: Clone + ToSpan + FromCst<rnix::ast::Inherit> {
    type Expr: self::Expr;
    type Attr: self::Attr;
    fn namespace(&self) -> Option<Self::Expr>;
    fn attrs(&self) -> Vec<Self::Attr>;
    fn new(namespace: Option<Self::Expr>, attrs: Vec<Self::Attr>, span: Span) -> Self;
}

pub trait AttrPath: Clone + FromCst<rnix::ast::Attrpath> {
    type Attr: self::Attr;
    fn first(&mut self) -> Option<&Self::Attr>;
    fn take_first(&mut self) -> Option<Self::Attr>;
}

pub trait AttrPathValue: Clone + ToSpan + FromCst<rnix::ast::AttrpathValue> {
    type Expr: Expr;
    type AttrPath: AttrPath;
    fn path(&self) -> Self::AttrPath;
    fn value(&self) -> Self::Expr;
    fn new(span: Span, path: Self::AttrPath, value: Self::Expr) -> Self;
}

pub trait Expr: ToSpan + FromCst<ast::Expr> + Clone {
    fn to_cst(self) -> rnix::ast::Expr;
}

pub trait HasEntry {
    type Syntax: self::Syntax;
    fn inherits(&self, c: &Compiler) -> Vec<<Self::Syntax as self::Syntax>::Inherit>;
    fn attributes<'a>(&self, c: &Compiler) -> Vec<<Self::Syntax as self::Syntax>::AttrPathValue>;
}
