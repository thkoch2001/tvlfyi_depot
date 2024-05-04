///!
///! An implementation of syn::Syntax implementation using the rnix-parser CST
///!
///! This file is entirely boilerplate; there is nothing interesting going on
///! here.  Once the conversion to syn::Syntax is complete I will write macros
///! to generate all of the boilerplate from something that is less tedious to
///! read.
///!
///! Note: implementing syn::Syntax directly on the rnix::ast types, with no
///! wrappers or conversions of any kind, is not a goal.  Trying to do that
///! would make the syn::Syntax API extremely awkward.  Main reason why this
///! is the case:
///!
///! 1. When implementing a trait method which returns `&Option<T>`, you can't
///!    return `&Some(self.xyz)` because of the borrow checker.  This problem
///!    hapens for all enums, not just for `Option`.  This means that in order
///!    for Syntax to be implementable using entirely #[repr(transparent)]
///!    structs, none of its methods would be able to return references!  So
///!    every accessor method (like LetIn::body()) would have to clone() that
///!    part of the AST, on every access.  This is a problem, generally, with
///!    the Rust borrow checker -- you have to choose between reallocating
///!    wrappers or clone-on-access wrappers.
///!
use super::*;

// Zero-sized type.
pub struct SyntaxImpl {}

impl Syntax for SyntaxImpl {
    type AttrName = self::AttrNameImpl;
    type AttrPath = self::AttrPathImpl;
    type AttrPathValue = self::AttrPathValueImpl;
    type Expr = self::ExprImpl;
    type Inherit = self::InheritImpl;
    type LetIn = self::LetInImpl;
    type AttrSet = self::AttrSetImpl;
    type LegacyLet = self::LegacyLetImpl;
    type Ident = self::IdentImpl;
    type Entry = self::EntryImpl;
}

#[derive(Clone)]
pub enum EntryImpl {
    Inherit(InheritImpl),
    AttrPathValue(AttrPathValueImpl),
}

impl ToSpan for EntryImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        match self {
            EntryImpl::Inherit(inherit) => inherit.span_for(file),
            EntryImpl::AttrPathValue(attr_path_value) => attr_path_value.span_for(file),
        }
    }
}

impl FromCst<rnix::ast::Entry> for EntryImpl {
    fn from_cst(entry: rnix::ast::Entry) -> Self {
        match entry {
            rnix::ast::Entry::Inherit(inherit) => InheritImpl::from_cst(inherit).into(),
            rnix::ast::Entry::AttrpathValue(attr_path_value) => {
                AttrPathValueImpl::from_cst(attr_path_value).into()
            }
        }
    }
}

impl From<AttrPathValueImpl> for EntryImpl {
    fn from(attr_path_value: AttrPathValueImpl) -> Self {
        EntryImpl::AttrPathValue(attr_path_value)
    }
}

impl From<InheritImpl> for EntryImpl {
    fn from(inherit: InheritImpl) -> Self {
        EntryImpl::Inherit(inherit)
    }
}

impl Entry<SyntaxImpl> for EntryImpl {
    fn as_inherit(&self) -> Option<&InheritImpl> {
        match self {
            EntryImpl::Inherit(inherit) => Some(inherit),
            _ => None,
        }
    }
    fn as_attr_path_value(&self) -> Option<&AttrPathValueImpl> {
        match self {
            EntryImpl::AttrPathValue(attr_path_value) => Some(attr_path_value),
            _ => None,
        }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct AttrPathImpl {
    components: Vec<AttrNameImpl>,
}

impl AttrPath<SyntaxImpl> for AttrPathImpl {
    fn first(&self) -> Option<&AttrNameImpl> {
        self.components.first()
    }
}

impl FromIterator<AttrNameImpl> for AttrPathImpl {
    fn from_iter<Iter>(iter: Iter) -> Self
    where
        Iter: IntoIterator<Item = AttrNameImpl>,
    {
        let components: Vec<_> = iter.into_iter().collect();
        AttrPathImpl { components }
    }
}

impl FromCst<rnix::ast::Attrpath> for AttrPathImpl {
    fn from_cst(entry: rnix::ast::Attrpath) -> Self {
        let coll: Vec<_> = entry.attrs().map(|a| <AttrNameImpl>::from_cst(a)).collect();
        AttrPathImpl { components: coll }
    }
}

impl IntoIterator for AttrPathImpl {
    type Item = AttrNameImpl;
    type IntoIter = <Vec<AttrNameImpl> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.components.into_iter()
    }
}

impl ToSpan for AttrPathImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.components
            .iter()
            .map(|name| name.span_for(file))
            .reduce(|span1, span2| span1.merge(span2))
            .expect("AttrPath with zero components")
    }
}

#[derive(Clone)]
pub struct AttrPathValueImpl {
    path: AttrPathImpl,
    value: ExprImpl,
}

impl FromCst<rnix::ast::AttrpathValue> for AttrPathValueImpl {
    fn from_cst(entry: rnix::ast::AttrpathValue) -> Self {
        AttrPathValueImpl {
            path: if let Some(attrpath) = entry.attrpath() {
                AttrPathImpl::from_cst(attrpath)
            } else {
                AttrPathImpl { components: vec![] }
            },
            value: ExprImpl::from_cst(entry.value().unwrap()),
        }
    }
}

impl ToSpan for AttrPathValueImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.path.span_for(file).merge(self.value.span_for(file))
    }
}

impl AttrPathValue<SyntaxImpl> for AttrPathValueImpl {
    fn path(&self) -> &<SyntaxImpl as Syntax>::AttrPath {
        &self.path
    }
    fn value(&self) -> &<SyntaxImpl as Syntax>::Expr {
        &self.value
    }
    fn into_value(self) -> <SyntaxImpl as Syntax>::Expr {
        self.value
    }
    fn new(path: <SyntaxImpl as Syntax>::AttrPath, value: ExprImpl) -> Self {
        AttrPathValueImpl { path, value }
    }
}

#[derive(Clone)]
pub struct InheritImpl {
    // TODO(amjoseph): the only reason this exists is to process bogus empty
    // `inherit;` statements.  We should just drop them from the AST instead.
    inherit_token: Option<rnix::SyntaxToken>,
    namespace: Option<ExprImpl>,
    attrs: Vec<<SyntaxImpl as Syntax>::Ident>,
}

impl FromCst<rnix::ast::Inherit> for InheritImpl {
    fn from_cst(entry: rnix::ast::Inherit) -> Self {
        let inherit_token = entry.inherit_token();
        let namespace = entry
            .from()
            .map(|from| from.expr())
            .flatten()
            .map(|e| ExprImpl::from_cst(e));
        let attrs: Vec<IdentImpl> = entry
            .attrs()
            .map(|cst| Some(IdentImpl::Attr(cst)))
            .filter_map(|i| i)
            .collect();
        InheritImpl {
            inherit_token,
            namespace,
            attrs,
        }
    }
}

impl Inherit<SyntaxImpl> for InheritImpl {
    fn namespace(&self) -> &Option<ExprImpl> {
        &self.namespace
    }
    fn attrs<'a>(&'a self) -> impl Iterator<Item = &'a <SyntaxImpl as Syntax>::Ident> {
        self.attrs.iter()
    }
    fn new(
        namespace: Option<<SyntaxImpl as Syntax>::Expr>,
        attrs: impl Iterator<Item = <SyntaxImpl as Syntax>::Ident>,
    ) -> Self {
        InheritImpl {
            inherit_token: None,
            namespace,
            attrs: attrs.collect(),
        }
    }
}

impl ToSpan for InheritImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.inherit_token
            .iter()
            .map(|tok| tok.span_for(file))
            .chain(
                self.namespace
                    .iter()
                    .map(|namespace| namespace.span_for(file)),
            )
            .chain(self.attrs.iter().map(|name| name.span_for(file)))
            .reduce(|span1, span2| span1.merge(span2))
            .expect("nothing to get a span from")
    }
}

#[derive(Clone)]
pub enum ExprImpl {
    Cst(rnix::ast::Expr),
    AttrSet(AttrSetImpl),
}

impl ToSpan for ExprImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        match self {
            ExprImpl::Cst(cst) => cst.span_for(file),
            ExprImpl::AttrSet(attr_set_impl) => attr_set_impl.span_for(file),
        }
    }
}

impl Expr<SyntaxImpl> for ExprImpl {
    fn to_cst(self) -> rnix::ast::Expr {
        match self {
            ExprImpl::Cst(cst) => cst,
            ExprImpl::AttrSet(attr_set_impl) => attr_set_impl.to_cst(),
        }
    }
    fn as_attrset(&self) -> Option<&AttrSetImpl> {
        match self {
            ExprImpl::AttrSet(attrset) => Some(&attrset),
            _ => None,
        }
    }
}

impl FromCst<rnix::ast::Expr> for ExprImpl {
    fn from_cst(cst: rnix::ast::Expr) -> Self {
        match cst {
            rnix::ast::Expr::AttrSet(cst) => ExprImpl::AttrSet(AttrSetImpl::from_cst(cst)),
            _ => ExprImpl::Cst(cst),
        }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct LetInImpl {
    cst: rnix::ast::LetIn,
}

impl ToSpan for LetInImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.cst.span_for(file)
    }
}

impl LetIn<SyntaxImpl> for LetInImpl {
    fn body(self) -> <SyntaxImpl as Syntax>::Expr {
        ExprImpl::from_cst(self.cst.body().clone().unwrap())
    }
}

impl FromCst<rnix::ast::LetIn> for LetInImpl {
    fn from_cst(cst: rnix::ast::LetIn) -> Self {
        LetInImpl { cst }
    }
}

impl HasEntries<SyntaxImpl> for LetInImpl {
    fn into_iter(&self) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.cst).map(|entry| EntryImpl::from_cst(entry))
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct AttrSetImpl {
    cst: rnix::ast::AttrSet,
}

impl ToSpan for AttrSetImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.cst.span_for(file)
    }
}

impl HasEntries<SyntaxImpl> for AttrSetImpl {
    fn into_iter(&self) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.cst).map(|entry| EntryImpl::from_cst(entry))
    }
}

impl AttrSet<SyntaxImpl> for AttrSetImpl {
    fn is_rec(&self) -> bool {
        self.cst.rec_token().is_some()
    }
    fn to_cst(self) -> rnix::ast::Expr {
        rnix::ast::Expr::AttrSet(self.cst)
    }
}

impl FromCst<rnix::ast::AttrSet> for AttrSetImpl {
    fn from_cst(cst: rnix::ast::AttrSet) -> Self {
        AttrSetImpl { cst }
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct LegacyLetImpl {
    cst: rnix::ast::LegacyLet,
}

impl LegacyLet<SyntaxImpl> for LegacyLetImpl {}

impl HasEntries<SyntaxImpl> for LegacyLetImpl {
    fn into_iter(&self) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.cst).map(|entry| EntryImpl::from_cst(entry))
    }
}

impl FromCst<rnix::ast::LegacyLet> for LegacyLetImpl {
    fn from_cst(cst: rnix::ast::LegacyLet) -> Self {
        LegacyLetImpl { cst }
    }
}

impl ToSpan for LegacyLetImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        self.cst.span_for(&file)
    }
}

#[derive(Clone)]
pub enum IdentImpl {
    Cst(rnix::ast::Ident),
    Attr(rnix::ast::Attr),

    // This variant is used to allow synthesizing an IdentImpl, since we
    // can't synthesize rnix::ast::Ident.
    String { span: Span, ident: String },
}

impl Ident<SyntaxImpl> for IdentImpl {
    fn ident(&self) -> smol_str::SmolStr {
        match self {
            IdentImpl::Cst(ident) => ident.ident_token().unwrap().text().into(),
            IdentImpl::Attr(attr) => {
                expr_static_attr_str(attr).expect("IdentImpl::Attr on a dynamic string")
            }
            IdentImpl::String { ident, .. } => ident.into(),
        }
    }
    fn new(ident: String, span: Span) -> Self {
        IdentImpl::String { ident, span }
    }
}

impl ToSpan for IdentImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        match self {
            IdentImpl::Cst(ident) => ident.span_for(file),
            IdentImpl::Attr(attr) => attr.span_for(file),
            IdentImpl::String { span, .. } => *span,
        }
    }
}

impl FromCst<rnix::ast::Ident> for IdentImpl {
    fn from_cst(cst: rnix::ast::Ident) -> Self {
        IdentImpl::Cst(cst)
    }
}

impl FromCst<rnix::ast::Attr> for IdentImpl {
    fn from_cst(cst: rnix::ast::Attr) -> Self {
        IdentImpl::Attr(cst)
    }
}

#[derive(Clone)]
pub enum AttrNameImpl {
    Static(IdentImpl),
    Dynamic(ExprImpl),
}

impl FromCst<rnix::ast::Attr> for AttrNameImpl {
    fn from_cst(cst: rnix::ast::Attr) -> Self {
        if expr_static_attr_str(&cst).is_some() {
            AttrNameImpl::Static(IdentImpl::from_cst(cst))
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
            AttrNameImpl::Dynamic(ExprImpl::from_cst(expr))
        }
    }
}

impl ToSpan for AttrNameImpl {
    fn span_for(&self, file: &codemap::File) -> Span {
        match self {
            AttrNameImpl::Static(attrname_static) => attrname_static.span_for(file),
            AttrNameImpl::Dynamic(attrname_dynamic) => attrname_dynamic.span_for(file),
        }
    }
}

/// Least common supertrait of `Static` and `Dynamic`
impl AttrName<SyntaxImpl> for AttrNameImpl {
    fn as_static(&self) -> Option<&IdentImpl> {
        match self {
            AttrNameImpl::Static(inner) => Some(inner),
            _ => None,
        }
    }
    fn as_dynamic(&self) -> Option<&ExprImpl> {
        match self {
            AttrNameImpl::Dynamic(inner) => Some(inner),
            _ => None,
        }
    }
}

impl From<ExprImpl> for AttrNameImpl {
    fn from(expr: ExprImpl) -> Self {
        AttrNameImpl::Dynamic(expr)
    }
}

impl From<IdentImpl> for AttrNameImpl {
    fn from(ident: IdentImpl) -> Self {
        AttrNameImpl::Static(ident)
    }
}
