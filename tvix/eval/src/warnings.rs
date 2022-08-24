//! Implements warnings that are emitted in cases where code passed to
//! Tvix exhibits problems that the user could address.

#[derive(Debug)]
pub enum WarningKind {
    DeprecatedLiteralURL,
    UselessInherit,
    UnusedBinding,
    ShadowedGlobal(&'static str),
}

#[derive(Debug)]
pub struct EvalWarning {
    pub node: rnix::SyntaxNode,
    pub kind: WarningKind,
}
