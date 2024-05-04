///!
///! An implementation of syn::Syntax implementation using the rnix-parser CST
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
    fn from_cst(c: &mut Compiler, entry: rnix::ast::Entry) -> Self {
        match entry {
            rnix::ast::Entry::Inherit(inherit) => InheritImpl::from_cst(c, inherit).into(),
            rnix::ast::Entry::AttrpathValue(attr_path_value) => {
                AttrPathValueImpl::from_cst(c, attr_path_value).into()
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
    fn from_cst(c: &mut Compiler, entry: rnix::ast::Attrpath) -> Self {
        let coll: Vec<_> = entry
            .attrs()
            .map(|a| <AttrNameImpl>::from_cst(c, a))
            .collect();
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

#[derive(Clone)]
pub struct AttrPathValueImpl {
    pub span: Span,
    pub remaining_path: AttrPathImpl,
    pub value: ExprImpl,
}

impl FromCst<rnix::ast::AttrpathValue> for AttrPathValueImpl {
    fn from_cst(c: &mut Compiler, entry: rnix::ast::AttrpathValue) -> Self {
        AttrPathValueImpl {
            span: entry.span_for(c.file),
            remaining_path: if let Some(attrpath) = entry.attrpath() {
                AttrPathImpl::from_cst(c, attrpath)
            } else {
                AttrPathImpl { components: vec![] }
            },
            value: ExprImpl::from_cst(c, entry.value().unwrap()),
        }
    }
}

impl ToSpan for AttrPathValueImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl AttrPathValue<SyntaxImpl> for AttrPathValueImpl {
    fn path(&self) -> <SyntaxImpl as Syntax>::AttrPath {
        self.remaining_path.clone()
    }
    fn value(&self) -> <SyntaxImpl as Syntax>::Expr {
        self.value.clone()
    }
    fn new(span: Span, remaining_path: <SyntaxImpl as Syntax>::AttrPath, value: ExprImpl) -> Self {
        AttrPathValueImpl {
            span,
            remaining_path,
            value,
        }
    }
}

#[derive(Clone)]
pub struct InheritImpl {
    namespace: Option<ExprImpl>,
    attrs: Vec<<SyntaxImpl as Syntax>::Ident>,
    span: Span,
}

impl FromCst<rnix::ast::Inherit> for InheritImpl {
    fn from_cst(c: &mut Compiler, entry: rnix::ast::Inherit) -> Self {
        let namespace = entry
            .from()
            .map(|from| from.expr())
            .flatten()
            .map(|e| ExprImpl::from_cst(c, e));
        let attrs: Vec<IdentImpl> = entry
            .attrs()
            .map(|cst| {
                if let Some(s) = expr_static_attr_str(&cst) {
                    Some(IdentImpl::new(s.into(), c.span_for(&entry)))
                } else {
                    c.emit_error(&cst, ErrorKind::DynamicKeyInScope("inherit"));
                    None
                }
            })
            .filter_map(|i| i)
            .collect();
        let span = c.span_for(&entry);
        InheritImpl {
            namespace,
            attrs,
            span,
        }
    }
}

impl Inherit<SyntaxImpl> for InheritImpl {
    fn namespace(&self) -> Option<ExprImpl> {
        self.namespace.clone()
    }
    fn attrs<'a>(&'a self) -> impl Iterator<Item = &'a <SyntaxImpl as Syntax>::Ident> {
        self.attrs.iter()
    }
    fn new(
        namespace: Option<<SyntaxImpl as Syntax>::Expr>,
        attrs: impl Iterator<Item = <SyntaxImpl as Syntax>::Ident>,
        span: Span,
    ) -> Self {
        InheritImpl {
            namespace,
            attrs: attrs.collect(),
            span,
        }
    }
}

impl ToSpan for InheritImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
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
    fn from_cst(c: &mut Compiler, cst: rnix::ast::Expr) -> Self {
        match cst {
            rnix::ast::Expr::AttrSet(cst) => ExprImpl::AttrSet(AttrSetImpl::from_cst(c, cst)),
            _ => ExprImpl::Cst(cst),
        }
    }
}

#[derive(Clone)]
pub struct LetInImpl {
    span: Span,
    entry: rnix::ast::LetIn,
    body: ExprImpl,
}

impl ToSpan for LetInImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl LetIn<SyntaxImpl> for LetInImpl {
    fn body(self) -> <SyntaxImpl as Syntax>::Expr {
        self.body
    }
}

impl FromCst<rnix::ast::LetIn> for LetInImpl {
    fn from_cst(c: &mut Compiler, entry: rnix::ast::LetIn) -> Self {
        LetInImpl {
            span: c.span_for(&entry),
            body: ExprImpl::from_cst(c, entry.body().clone().unwrap()),
            entry: entry,
        }
    }
}

impl HasEntries<SyntaxImpl> for LetInImpl {
    fn into_iter(&self, c: &mut Compiler) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.entry).map(|entry| EntryImpl::from_cst(c, entry))
    }
}

#[derive(Clone)]
pub struct AttrSetImpl {
    is_rec: bool,
    span: Span,
    entry: rnix::ast::AttrSet,
}

impl ToSpan for AttrSetImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl HasEntries<SyntaxImpl> for AttrSetImpl {
    fn into_iter(&self, c: &mut Compiler) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.entry).map(|entry| EntryImpl::from_cst(c, entry))
    }
}

impl AttrSet<SyntaxImpl> for AttrSetImpl {
    fn is_rec(&self) -> bool {
        self.is_rec
    }
    fn to_cst(self) -> rnix::ast::Expr {
        rnix::ast::Expr::AttrSet(self.entry)
    }
}

impl FromCst<rnix::ast::AttrSet> for AttrSetImpl {
    fn from_cst(c: &mut Compiler, entry: rnix::ast::AttrSet) -> Self {
        AttrSetImpl {
            span: c.span_for(&entry),
            is_rec: entry.rec_token().is_some(),
            entry,
        }
    }
}

#[derive(Clone)]
pub struct LegacyLetImpl {
    span: Span,
    entry: rnix::ast::LegacyLet,
}

impl LegacyLet<SyntaxImpl> for LegacyLetImpl {}

impl HasEntries<SyntaxImpl> for LegacyLetImpl {
    fn into_iter(&self, c: &mut Compiler) -> impl Iterator<Item = EntryImpl> {
        rnix::ast::HasEntry::entries(&self.entry).map(|entry| EntryImpl::from_cst(c, entry))
    }
}

impl FromCst<rnix::ast::LegacyLet> for LegacyLetImpl {
    fn from_cst(c: &mut Compiler, entry: rnix::ast::LegacyLet) -> Self {
        LegacyLetImpl {
            span: c.span_for(&entry),
            entry,
        }
    }
}

impl ToSpan for LegacyLetImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

#[derive(Clone)]
pub struct IdentImpl {
    span: Span,
    ident: String,
}

impl Ident<SyntaxImpl> for IdentImpl {
    fn ident(&self) -> &str {
        &self.ident
    }
    fn new(ident: String, span: Span) -> Self {
        IdentImpl { ident, span }
    }
}

impl ToSpan for IdentImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl FromCst<rnix::ast::Ident> for IdentImpl {
    fn from_cst(c: &mut Compiler, ident: rnix::ast::Ident) -> Self {
        IdentImpl {
            span: c.span_for(&ident),
            ident: ident.to_string(),
        }
    }
}

#[derive(Clone)]
pub enum AttrNameImpl {
    Static(IdentImpl),
    Dynamic(ExprImpl),
}

impl FromCst<rnix::ast::Attr> for AttrNameImpl {
    fn from_cst(c: &mut Compiler, cst: rnix::ast::Attr) -> Self {
        let span = c.span_for(&cst);
        if let Some(s) = expr_static_attr_str(&cst) {
            AttrNameImpl::Static(IdentImpl::new(s.into(), span))
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
            AttrNameImpl::Dynamic(ExprImpl::from_cst(c, expr))
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
