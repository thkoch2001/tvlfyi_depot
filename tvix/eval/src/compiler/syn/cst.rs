///!
///! An implementation of syn::Syntax implementation using the rnix-parser CST
///!
use super::*;

// Zero-sized type.
pub struct SyntaxImpl {}

impl Syntax for SyntaxImpl {
    type AttrPath = self::AttrPathImpl;
    type AttrPathValue = self::AttrPathValueImpl<self::AttrPathImpl>;
    type Expr = rnix::ast::Expr;
    type Inherit = self::InheritImpl;
    type Attr = self::AttrImpl;
    type LetIn = self::LetInImpl;
    type AttrSet = self::AttrSetImpl;
    type LegacyLet = self::LegacyLetImpl;
    type Ident = self::IdentImpl;
}

#[derive(Clone)]
pub struct AttrPathImpl {
    // Note: these are in reverse order to allow the use of Vec::pop()
    components: Vec<AttrImpl>,
}

impl AttrPath for AttrPathImpl {
    type Attr = self::AttrImpl;
    fn first(&mut self) -> Option<&Self::Attr> {
        self.components.last()
    }
    fn take_first(&mut self) -> Option<Self::Attr> {
        self.components.pop()
    }
}

impl FromCst<rnix::ast::Attrpath> for AttrPathImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::Attrpath) -> Self {
        let mut coll: Vec<_> = entry.attrs().map(|a| AttrImpl::from_cst(c, a)).collect();
        coll.reverse();
        AttrPathImpl { components: coll }
    }
}

#[derive(Clone)]
pub struct AttrPathValueImpl<AttrPath> {
    pub span: Span,
    pub remaining_path: AttrPath,
    pub value: ast::Expr,
}

impl FromCst<rnix::ast::AttrpathValue> for AttrPathValueImpl<AttrPathImpl> {
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

impl<AttrPath> ToSpan for AttrPathValueImpl<AttrPath> {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}

impl AttrPathValue for AttrPathValueImpl<AttrPathImpl> {
    type AttrPath = AttrPathImpl;
    type Expr = <SyntaxImpl as Syntax>::Expr;
    fn path(&self) -> Self::AttrPath {
        self.remaining_path.clone()
    }
    fn value(&self) -> Self::Expr {
        self.value.clone()
    }
    fn new(span: Span, remaining_path: Self::AttrPath, value: ast::Expr) -> Self {
        AttrPathValueImpl {
            span,
            remaining_path,
            value,
        }
    }
}

impl<N: rnix::ast::HasEntry> HasEntry for N {
    type Syntax = self::SyntaxImpl;
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(self)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl<AttrPathImpl>> {
        rnix::ast::HasEntry::attrpath_values(self)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
    }
}

#[derive(Clone)]
pub struct InheritImpl {
    namespace: Option<ast::Expr>,
    attrs: Vec<AttrImpl>,
    span: Span,
}

impl FromCst<rnix::ast::Inherit> for InheritImpl {
    fn from_cst(c: &Compiler, entry: rnix::ast::Inherit) -> Self {
        let namespace = entry.from().map(|from| from.expr()).flatten();
        let attrs: Vec<AttrImpl> = entry
            .attrs()
            .map(|node| AttrImpl::from_cst(c, node))
            .collect();
        let span = c.span_for(&entry);
        InheritImpl {
            namespace,
            attrs,
            span,
        }
    }
}

impl Inherit for InheritImpl {
    type Expr = <SyntaxImpl as Syntax>::Expr;
    type Attr = <SyntaxImpl as Syntax>::Attr;
    fn namespace(&self) -> Option<ast::Expr> {
        self.namespace.clone()
    }
    fn attrs(&self) -> Vec<Self::Attr> {
        self.attrs.clone()
    }
    fn new(namespace: Option<Self::Expr>, attrs: Vec<Self::Attr>, span: Span) -> Self {
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

impl Expr for rnix::ast::Expr {
    type Syntax = SyntaxImpl;
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
pub struct AttrImpl {
    span: Span,
    attr: StaticOrDynamic<SmolStr, rnix::ast::Expr>,
}
impl Attr for AttrImpl {
    type Expr = <SyntaxImpl as Syntax>::Expr;
    fn attr(&self) -> &StaticOrDynamic<SmolStr, Self::Expr> {
        &self.attr
    }
    fn into_attr(self) -> StaticOrDynamic<SmolStr, Self::Expr> {
        self.attr
    }
    fn from_static(str: SmolStr, span: Span) -> Self {
        AttrImpl {
            span,
            attr: StaticOrDynamic::Static(str),
        }
    }
    fn from_dynamic(expr: Self::Expr, span: Span) -> Self {
        AttrImpl {
            span,
            attr: StaticOrDynamic::Dynamic(expr),
        }
    }
}
impl ToSpan for AttrImpl {
    fn span_for(&self, _file: &codemap::File) -> Span {
        self.span
    }
}
impl FromCst<rnix::ast::Attr> for AttrImpl {
    fn from_cst(c: &Compiler, cst: rnix::ast::Attr) -> Self {
        let span = c.span_for(&cst);
        AttrImpl {
            span,
            attr: if let Some(s) = expr_static_attr_str(&cst) {
                StaticOrDynamic::Static(s.into())
            } else {
                StaticOrDynamic::Dynamic(match cst {
                    ast::Attr::Dynamic(expr) => {
                        <AttrImpl as Attr>::Expr::from_cst(c, expr.expr().unwrap())
                    }
                    ast::Attr::Str(str) => {
                        <AttrImpl as Attr>::Expr::from_cst(c, rnix::ast::Expr::Str(str))
                    }
                    ast::Attr::Ident(_expr) => {
                        unreachable!(
                            "if you see this, it means that expr_static_attr_str() has a bug in it"
                        )
                    }
                })
            },
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

impl LetIn for LetInImpl {
    type Expr = <SyntaxImpl as Syntax>::Expr;
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

impl HasEntry for LetInImpl {
    type Syntax = self::SyntaxImpl;
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl<AttrPathImpl>> {
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

impl HasEntry for AttrSetImpl {
    type Syntax = self::SyntaxImpl;
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl<AttrPathImpl>> {
        rnix::ast::HasEntry::attrpath_values(&self.entry)
            .map(move |entry| AttrPathValueImpl::from_cst(c, entry))
            .collect()
    }
}

impl AttrSet for AttrSetImpl {
    type AttrPath = AttrPathImpl;
    type AttrPathValue = AttrPathValueImpl<Self::AttrPath>;
    type Inherit = InheritImpl;
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

impl LegacyLet for LegacyLetImpl {}

impl HasEntry for LegacyLetImpl {
    type Syntax = self::SyntaxImpl;
    fn inherits(&self, c: &Compiler) -> Vec<InheritImpl> {
        rnix::ast::HasEntry::inherits(&self.entry)
            .into_iter()
            .map(|inherit| InheritImpl::from_cst(c, inherit))
            .collect()
    }

    fn attributes<'a>(&self, c: &'a Compiler) -> Vec<AttrPathValueImpl<AttrPathImpl>> {
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

impl Ident for IdentImpl {
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
