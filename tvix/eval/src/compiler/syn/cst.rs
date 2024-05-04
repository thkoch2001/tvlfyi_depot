///!
///! An implementation of syn::Syntax implementation using the rnix-parser CST
///!
use super::*;

// Zero-sized type.
pub struct SyntaxImpl {}

impl Syntax for SyntaxImpl {
    type AttrPath = self::AttrPathImpl;
    type AttrPathValue = self::AttrPathValueImpl;
    type Expr = rnix::ast::Expr;
    type Inherit = self::InheritImpl;
    type AttrNameStatic = self::AttrNameStaticImpl;
    type AttrNameDynamic = self::AttrNameDynamicImpl;
    type LetIn = self::LetInImpl;
    type AttrSet = self::AttrSetImpl;
    type LegacyLet = self::LegacyLetImpl;
    type Ident = self::IdentImpl;
    type Entry = self::EntryImpl;
}

pub struct EntryImpl {}

impl Entry<SyntaxImpl> for EntryImpl {}

#[derive(Clone)]
pub struct AttrPathImpl {
    // Note: these are in reverse order to allow the use of Vec::pop()
    components: Vec<AttrName<SyntaxImpl>>,
}

impl AttrPath<SyntaxImpl> for AttrPathImpl {
    fn first(&mut self) -> Option<&AttrName<SyntaxImpl>> {
        self.components.last()
    }
    fn take_first(&mut self) -> Option<AttrName<SyntaxImpl>> {
        self.components.pop()
    }
}

impl FromCst<rnix::ast::Attrpath> for AttrPathImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::Attrpath) -> Self {
        let mut coll: Vec<_> = entry
            .attrs()
            .map(|a| <AttrName<SyntaxImpl>>::from_cst(c, a))
            .collect();
        coll.reverse();
        AttrPathImpl { components: coll }
    }
}

#[derive(Clone)]
pub struct AttrPathValueImpl {
    pub span: Span,
    pub remaining_path: AttrPathImpl,
    pub value: ast::Expr,
}

impl FromCst<rnix::ast::AttrpathValue> for AttrPathValueImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::AttrpathValue) -> Self {
        AttrPathValueImpl {
            span: entry.span_for(c.file),
            remaining_path: if let Some(attrpath) = entry.attrpath() {
                AttrPathImpl::from_cst(c, attrpath)
            } else {
                AttrPathImpl { components: vec![] }
            },
            value: entry.value().unwrap(),
        }
    }
}

impl ToSpan for AttrPathValueImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl Entry<SyntaxImpl> for AttrPathValueImpl {}

impl AttrPathValue<SyntaxImpl> for AttrPathValueImpl {
    fn path(&self) -> <SyntaxImpl as Syntax>::AttrPath {
        self.remaining_path.clone()
    }
    fn value(&self) -> <SyntaxImpl as Syntax>::Expr {
        self.value.clone()
    }
    fn new(span: Span, remaining_path: <SyntaxImpl as Syntax>::AttrPath, value: ast::Expr) -> Self {
        AttrPathValueImpl {
            span,
            remaining_path,
            value,
        }
    }
}

impl<N: rnix::ast::HasEntry> HasEntries<SyntaxImpl> for N {
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(self)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl> {
        rnix::ast::HasEntry::attrpath_values(self)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
    }
}

#[derive(Clone)]
pub struct InheritImpl {
    namespace: Option<ast::Expr>,
    attrs: Vec<AttrName<SyntaxImpl>>,
    span: Span,
}

impl Entry<SyntaxImpl> for InheritImpl {}

impl FromCst<rnix::ast::Inherit> for InheritImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::Inherit) -> Self {
        let namespace = entry.from().map(|from| from.expr()).flatten();
        let attrs: Vec<AttrName<SyntaxImpl>> = entry
            .attrs()
            .map(|node| <AttrName<SyntaxImpl>>::from_cst(c, node))
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
    fn namespace(&self) -> Option<ast::Expr> {
        self.namespace.clone()
    }
    fn attrs(&self) -> Vec<AttrName<SyntaxImpl>> {
        self.attrs.clone()
    }
    fn new(
        namespace: Option<<SyntaxImpl as Syntax>::Expr>,
        attrs: Vec<AttrName<SyntaxImpl>>,
        span: Span,
    ) -> Self {
        InheritImpl {
            namespace,
            attrs,
            span,
        }
    }
}

impl ToSpan for InheritImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl Expr<SyntaxImpl> for rnix::ast::Expr {
    fn as_cst(&self) -> &rnix::ast::Expr {
        &self
    }
    fn to_cst(self) -> rnix::ast::Expr {
        self
    }
    fn is_attrset(&self) -> bool {
        matches!(self.as_cst(), ast::Expr::AttrSet(_))
    }
    fn as_attrset(self, c: &Compiler) -> Option<AttrSetImpl> {
        match self {
            ast::Expr::AttrSet(attrset) => Some(AttrSetImpl::from_cst(c, attrset)),
            _ => None,
        }
    }
}

impl FromCst<rnix::ast::Expr> for rnix::ast::Expr {
    fn from_cst(_c: &Compiler, cst: rnix::ast::Expr) -> Self {
        cst
    }
}

#[derive(Clone)]
pub struct AttrNameStaticImpl {
    span: Span,
    attr: String,
}
#[derive(Clone)]
pub struct AttrNameDynamicImpl {
    span: Span,
    attr: rnix::ast::Expr,
}

impl Into<String> for AttrNameStaticImpl {
    fn into(self) -> String {
        self.attr
    }
}
impl AsRef<str> for AttrNameStaticImpl {
    fn as_ref(&self) -> &str {
        &self.attr
    }
}
impl AttrNameStatic<SyntaxImpl> for AttrNameStaticImpl {
    fn new(attr: String, span: Span) -> Self {
        AttrNameStaticImpl { attr, span }
    }
}
impl AttrNameDynamic<SyntaxImpl> for AttrNameDynamicImpl {
    fn new(attr: rnix::ast::Expr, span: Span) -> Self {
        AttrNameDynamicImpl { attr, span }
    }
    fn to_cst(self) -> rnix::ast::Expr {
        self.attr
    }
}
impl ToSpan for AttrNameStaticImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}
impl ToSpan for AttrNameDynamicImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}
impl FromCst<rnix::ast::Attr> for AttrNameStaticImpl {
    fn from_cst(c: &Compiler, attr: rnix::ast::Attr) -> Self {
        AttrNameStaticImpl {
            span: c.span_for(&attr),
            attr: attr.to_string(),
        }
    }
}

#[derive(Clone)]
pub struct LetInImpl {
    span: Span,
    entry: rnix::ast::LetIn,
}

impl ToSpan for LetInImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl LetIn<SyntaxImpl> for LetInImpl {
    fn body(self) -> <SyntaxImpl as Syntax>::Expr {
        self.entry.body().unwrap()
    }
}

impl FromCst<rnix::ast::LetIn> for LetInImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::LetIn) -> Self {
        LetInImpl {
            span: c.span_for(&entry),
            entry: entry,
        }
    }
}

impl HasEntries<SyntaxImpl> for LetInImpl {
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl> {
        rnix::ast::HasEntry::attrpath_values(&self.entry)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
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
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl> {
        rnix::ast::HasEntry::attrpath_values(&self.entry)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
    }
}

impl AttrSet<SyntaxImpl> for AttrSetImpl {
    fn is_rec(&self) -> bool {
        self.is_rec
    }
    fn to_cst(self) -> rnix::ast::AttrSet {
        self.entry
    }
}

impl FromCst<rnix::ast::AttrSet> for AttrSetImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::AttrSet) -> Self {
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
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl> {
        rnix::ast::HasEntry::attrpath_values(&self.entry)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
    }
}

impl FromCst<rnix::ast::LegacyLet> for LegacyLetImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::LegacyLet) -> Self {
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
}

impl ToSpan for IdentImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl FromCst<rnix::ast::Ident> for IdentImpl {
    fn from_cst(c: &Compiler, ident: rnix::ast::Ident) -> Self {
        IdentImpl {
            span: c.span_for(&ident),
            ident: ident.to_string(),
        }
    }
}
