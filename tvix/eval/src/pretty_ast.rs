//! Pretty-printed format for the rnix AST representation.
//!
//! The AST is serialised into a JSON structure that can then be
//! printed in either minimised or well-formatted style.

use std::fmt::Display;

use rnix::ast::{self, AstToken};
use serde::{
    ser::{SerializeMap, SerializeSeq},
    Serialize,
};

pub struct PrettyAST<'a>(&'a ast::Expr);

impl<'a> Serialize for PrettyAST<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serialize_expr(serializer, self.0)
    }
}

impl<'a> PrettyAST<'a> {
    pub fn from_expr(expr: &'a ast::Expr) -> Self {
        PrettyAST(expr)
    }

    pub fn pretty_string(&self) -> String {
        serde_json::ser::to_string_pretty(self).expect("AST serialisation should not fail")
    }
}

fn serialize_expr<S: serde::Serializer>(
    serializer: S,
    node: &ast::Expr,
) -> Result<S::Ok, S::Error> {
    match node {
        ast::Expr::Apply(node) => serialize_apply(serializer, node),
        ast::Expr::Assert(node) => serialize_assert(serializer, node),
        ast::Expr::IfElse(node) => serialize_if_else(serializer, node),
        ast::Expr::Select(node) => serialize_select(serializer, node),
        ast::Expr::Str(node) => serialize_str(serializer, node),
        ast::Expr::Path(node) => serialize_path(serializer, node),
        ast::Expr::Literal(node) => serialize_literal(serializer, node),
        ast::Expr::Lambda(node) => serialize_lambda(serializer, node),
        ast::Expr::LegacyLet(node) => serialize_legacy_let(serializer, node),
        ast::Expr::LetIn(node) => serialize_let_in(serializer, node),
        ast::Expr::List(node) => serialize_list(serializer, node),
        ast::Expr::BinOp(node) => serialize_binop(serializer, node),
        ast::Expr::Paren(node) => serialize_paren(serializer, node),
        ast::Expr::Root(node) => serialize_root(serializer, node),
        ast::Expr::AttrSet(node) => serialize_attrset(serializer, node),
        ast::Expr::UnaryOp(node) => serialize_unaryop(serializer, node),
        ast::Expr::Ident(node) => serialize_ident(serializer, node),
        ast::Expr::With(node) => serialize_with(serializer, node),
        ast::Expr::HasAttr(node) => serialize_hasattr(serializer, node),

        // We pass only correct ASTs to this formatter for now.
        ast::Expr::Error(_) => unreachable!(),
    }
}

fn serialize_apply<S: serde::Serializer>(
    serializer: S,
    node: &ast::Apply,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_assert<S: serde::Serializer>(
    serializer: S,
    node: &ast::Assert,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_if_else<S: serde::Serializer>(
    serializer: S,
    node: &ast::IfElse,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_select<S: serde::Serializer>(
    serializer: S,
    node: &ast::Select,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_str<S: serde::Serializer>(serializer: S, node: &ast::Str) -> Result<S::Ok, S::Error> {
    let parts = node.normalized_parts();
    let mut seq = serializer.serialize_seq(Some(parts.len()))?;

    for part in parts.into_iter() {
        match part {
            ast::InterpolPart::Literal(s) => seq.serialize_element(&s)?,
            ast::InterpolPart::Interpolation(expr) => {
                seq.serialize_element(&PrettyAST(&expr.expr().unwrap()))?
            }
        }
    }

    seq.end()
}

fn serialize_path<S: serde::Serializer>(
    serializer: S,
    node: &ast::Path,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_literal<S: serde::Serializer>(
    serializer: S,
    node: &ast::Literal,
) -> Result<S::Ok, S::Error> {
    match node.kind() {
        ast::LiteralKind::Float(val) => serializer.serialize_f64(val.value().unwrap()),
        ast::LiteralKind::Integer(val) => serializer.serialize_i64(val.value().unwrap()),
        ast::LiteralKind::Uri(val) => {
            let url = val.syntax().text();
            let mut map = serializer.serialize_map(Some(1))?;
            map.serialize_entry("url", url)?;
            map.end()
        }
    }
}

fn serialize_lambda<S: serde::Serializer>(
    serializer: S,
    node: &ast::Lambda,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_legacy_let<S: serde::Serializer>(
    serializer: S,
    node: &ast::LegacyLet,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_let_in<S: serde::Serializer>(
    serializer: S,
    node: &ast::LetIn,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_list<S: serde::Serializer>(
    serializer: S,
    node: &ast::List,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_binop<S: serde::Serializer>(
    serializer: S,
    node: &ast::BinOp,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_paren<S: serde::Serializer>(
    serializer: S,
    node: &ast::Paren,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_root<S: serde::Serializer>(
    serializer: S,
    node: &rnix::Root,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_attrset<S: serde::Serializer>(
    serializer: S,
    node: &ast::AttrSet,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_unaryop<S: serde::Serializer>(
    serializer: S,
    node: &ast::UnaryOp,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_ident<S: serde::Serializer>(
    serializer: S,
    node: &ast::Ident,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_with<S: serde::Serializer>(
    serializer: S,
    node: &ast::With,
) -> Result<S::Ok, S::Error> {
    todo!()
}

fn serialize_hasattr<S: serde::Serializer>(
    serializer: S,
    node: &ast::HasAttr,
) -> Result<S::Ok, S::Error> {
    todo!()
}

/*
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

        // We pass only correct ASTs to this formatter for now.
        ast::Expr::Error(_) => unreachable!(),
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
    write!(f, "AttrPath[ ")?;

    for attr in node.attrs() {
        match attr {
            ast::Attr::Ident(node) => pretty_ident(f, &node)?,
            ast::Attr::Dynamic(node) => pretty_dynamic(f, &node)?,
            ast::Attr::Str(node) => pretty_str(f, &node)?,
        };

        write!(f, " ")?;
    }

    write!(f, "]")
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
    use crate::value::nix_escape_string;

    write!(f, "Str[ ")?;

    for part in node.normalized_parts() {
        match part {
            ast::InterpolPart::Literal(s) => write!(f, "\"{}\"", nix_escape_string(&s))?,
            ast::InterpolPart::Interpolation(node) => pretty_expr(f, &node.expr().unwrap())?,
        };

        write!(f, " ")?;
    }

    write!(f, "]")
}

fn pretty_path(f: &mut std::fmt::Formatter, node: &ast::Path) -> std::fmt::Result {
    write!(f, "Path[ ")?;

    for part in node.parts() {
        match part {
            ast::InterpolPart::Literal(p) => write!(f, "{}", p.syntax().text())?,
            ast::InterpolPart::Interpolation(node) => pretty_expr(f, &node.expr().unwrap())?,
        };

        write!(f, " ")?;
    }

    write!(f, "]")
}

fn pretty_literal(f: &mut std::fmt::Formatter, node: &ast::Literal) -> std::fmt::Result {
    match node.kind() {
        ast::LiteralKind::Float(val) => write!(f, "Float({})", val.value().unwrap()),
        ast::LiteralKind::Integer(val) => write!(f, "Int({})", val.value().unwrap()),
        ast::LiteralKind::Uri(val) => write!(f, "Uri({})", val.syntax().text()),
    }
}

fn pretty_lambda(f: &mut std::fmt::Formatter, node: &ast::Lambda) -> std::fmt::Result {
    write!(f, "Lambda(")?;

    match node.param().unwrap() {
        ast::Param::IdentParam(node) => pretty_ident(f, &node.ident().unwrap())?,
        ast::Param::Pattern(node) => {
            write!(f, "Formals[")?;

            for (idx, pat) in node.pat_entries().enumerate() {
                if idx > 0 {
                    write!(f, ", ")?;
                }

                pretty_ident(f, &pat.ident().unwrap())?;

                if let Some(default) = pat.default() {
                    write!(f, "or {}", PrettyAST(default))?;
                }
            }

            if node.ellipsis_token().is_some() {
                write!(f, ", ...]")?;
            } else {
                write!(f, "]")?;
            }

            if let Some(bind) = node.pat_bind() {
                write!(
                    f,
                    "@{}",
                    bind.ident().unwrap().ident_token().unwrap().text()
                )?;
            }
        }
    }

    write!(f, ", {})", PrettyAST(node.body().unwrap()))
}

fn pretty_legacy_let(f: &mut std::fmt::Formatter, node: &ast::LegacyLet) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_let_in(f: &mut std::fmt::Formatter, node: &ast::LetIn) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_list(f: &mut std::fmt::Formatter, node: &ast::List) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_binop(f: &mut std::fmt::Formatter, node: &ast::BinOp) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_paren(f: &mut std::fmt::Formatter, node: &ast::Paren) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_root(f: &mut std::fmt::Formatter, node: &rnix::Root) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_attrset(f: &mut std::fmt::Formatter, node: &ast::AttrSet) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_unaryop(f: &mut std::fmt::Formatter, node: &ast::UnaryOp) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_ident(f: &mut std::fmt::Formatter, node: &ast::Ident) -> std::fmt::Result {
    write!(f, "Ident({})", node.ident_token().unwrap().text())
}

fn pretty_with(f: &mut std::fmt::Formatter, node: &ast::With) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_hasattr(f: &mut std::fmt::Formatter, node: &ast::HasAttr) -> std::fmt::Result {
    write!(f, "{}", node) // TODO!
}

fn pretty_dynamic(f: &mut std::fmt::Formatter, node: &ast::Dynamic) -> std::fmt::Result {
    write!(f, "Dynamic({})", PrettyAST(node.expr().unwrap()))
}
*/
