//! Pretty-printed format for the rnix AST representation.

use std::fmt::Display;

use rnix::ast;

struct PrettyAST(ast::Expr);

impl Display for PrettyAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        pretty_expr(f, &self.0)
    }
}

fn pretty_expr(f: &mut std::fmt::Formatter, node: &ast::Expr) -> std::fmt::Result {
    f.write_str("Expr(")?;

    match node {
        ast::Expr::Apply(node) => pretty_apply(f, node),
        ast::Expr::Assert(node) => pretty_assert(f, node),
        ast::Expr::IfElse(node) => pretty_if_else(f, node),
        ast::Expr::Select(node) => pretty_select(f, node),
        ast::Expr::Str(node) => pretty_str(f, node),
        ast::Expr::Path(node) => pretty_path(f, node),
        ast::Expr::Literal(node) => pretty_literal(f, node),
        ast::Expr::Lambda(node) => pretty_lambda(f, node),
        ast::Expr::LegacyLet(node) => pretty_legacy_let(f, node),
        ast::Expr::LetIn(node) => pretty_let_in(f, node),
        ast::Expr::List(node) => pretty_list(f, node),
        ast::Expr::BinOp(node) => pretty_binop(f, node),
        ast::Expr::Paren(node) => pretty_paren(f, node),
        ast::Expr::Root(node) => pretty_root(f, node),
        ast::Expr::AttrSet(node) => pretty_attrset(f, node),
        ast::Expr::UnaryOp(node) => pretty_unaryop(f, node),
        ast::Expr::Ident(node) => pretty_ident(f, node),
        ast::Expr::With(node) => pretty_with(f, node),
        ast::Expr::HasAttr(node) => pretty_hasattr(f, node),

        // We pass only correct ASTs to this formatter.
        ast::Expr::Error(node) => unreachable!(),
    }?;

    f.write_str(")")
}

fn pretty_apply(f: &mut std::fmt::Formatter, node: &ast::Apply) -> std::fmt::Result {
    write!(
        f,
        "Apply({}, {})",
        PrettyAST(node.lambda().unwrap()),
        PrettyAST(node.argument().unwrap())
    )
}

fn pretty_assert(f: &mut std::fmt::Formatter, node: &ast::Assert) -> std::fmt::Result {
    write!(
        f,
        "Assert({}, {})",
        PrettyAST(node.condition().unwrap()),
        PrettyAST(node.body().unwrap())
    )
}

fn pretty_if_else(f: &mut std::fmt::Formatter, node: &ast::IfElse) -> std::fmt::Result {
    write!(
        f,
        "IfElse({}, {}, {})",
        PrettyAST(node.condition().unwrap()),
        PrettyAST(node.body().unwrap()),
        PrettyAST(node.else_body().unwrap())
    )
}

fn pretty_attrpath(f: &mut std::fmt::Formatter, node: &ast::Attrpath) -> std::fmt::Result {
    todo!()
}

fn pretty_select(f: &mut std::fmt::Formatter, node: &ast::Select) -> std::fmt::Result {
    write!(f, "Select({}, ", PrettyAST(node.expr().unwrap()))?;
    pretty_attrpath(f, &node.attrpath().unwrap())?;

    if node.or_token().is_some() {
        write!(f, ", or = {})", PrettyAST(node.default_expr().unwrap()))
    } else {
        write!(f, ")")
    }
}

fn pretty_str(f: &mut std::fmt::Formatter, node: &ast::Str) -> std::fmt::Result {
    todo!()
}

fn pretty_path(f: &mut std::fmt::Formatter, node: &ast::Path) -> std::fmt::Result {
    todo!()
}

fn pretty_literal(f: &mut std::fmt::Formatter, node: &ast::Literal) -> std::fmt::Result {
    todo!()
}

fn pretty_lambda(f: &mut std::fmt::Formatter, node: &ast::Lambda) -> std::fmt::Result {
    todo!()
}

fn pretty_legacy_let(f: &mut std::fmt::Formatter, node: &ast::LegacyLet) -> std::fmt::Result {
    todo!()
}

fn pretty_let_in(f: &mut std::fmt::Formatter, node: &ast::LetIn) -> std::fmt::Result {
    todo!()
}

fn pretty_list(f: &mut std::fmt::Formatter, node: &ast::List) -> std::fmt::Result {
    todo!()
}

fn pretty_binop(f: &mut std::fmt::Formatter, node: &ast::BinOp) -> std::fmt::Result {
    todo!()
}

fn pretty_paren(f: &mut std::fmt::Formatter, node: &ast::Paren) -> std::fmt::Result {
    todo!()
}

fn pretty_root(f: &mut std::fmt::Formatter, node: &rnix::Root) -> std::fmt::Result {
    todo!()
}

fn pretty_attrset(f: &mut std::fmt::Formatter, node: &ast::AttrSet) -> std::fmt::Result {
    todo!()
}

fn pretty_unaryop(f: &mut std::fmt::Formatter, node: &ast::UnaryOp) -> std::fmt::Result {
    todo!()
}

fn pretty_ident(f: &mut std::fmt::Formatter, node: &ast::Ident) -> std::fmt::Result {
    todo!()
}

fn pretty_with(f: &mut std::fmt::Formatter, node: &ast::With) -> std::fmt::Result {
    todo!()
}

fn pretty_hasattr(f: &mut std::fmt::Formatter, node: &ast::HasAttr) -> std::fmt::Result {
    todo!()
}
