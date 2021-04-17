use std::collections::HashMap;
use std::mem;

use ast::hir::{Binding, Pattern};
use ast::Literal;
use void::{ResultVoidExt, Void};

use crate::ast::hir::{Decl, Expr};
use crate::ast::{self, Ident};

use super::Visitor;

/// Strip all values with a unit type in positive (non-return) position
pub(crate) struct StripPositiveUnits {}

impl<'a, 'ast> Visitor<'a, 'ast, ast::Type<'ast>> for StripPositiveUnits {
    type Error = Void;

    fn pre_visit_expr(
        &mut self,
        expr: &mut Expr<'ast, ast::Type<'ast>>,
    ) -> Result<(), Self::Error> {
        let mut extracted = vec![];
        if let Expr::Call { args, .. } = expr {
            // TODO(grfn): replace with drain_filter once it's stabilized
            let mut i = 0;
            while i != args.len() {
                if args[i].type_() == &ast::Type::Unit {
                    let expr = args.remove(i);
                    if !matches!(expr, Expr::Literal(Literal::Unit, _)) {
                        extracted.push(expr)
                    };
                } else {
                    i += 1
                }
            }
        }

        if !extracted.is_empty() {
            let body = mem::replace(expr, Expr::Literal(Literal::Unit, ast::Type::Unit));
            *expr = Expr::Let {
                bindings: extracted
                    .into_iter()
                    .map(|expr| Binding {
                        pat: Pattern::Id(
                            Ident::from_str_unchecked("___discarded"),
                            expr.type_().clone(),
                        ),
                        body: expr,
                    })
                    .collect(),
                type_: body.type_().clone(),
                body: Box::new(body),
            };
        }

        Ok(())
    }

    fn post_visit_call(
        &mut self,
        _fun: &mut Expr<'ast, ast::Type<'ast>>,
        _type_args: &mut HashMap<Ident<'ast>, ast::Type<'ast>>,
        args: &mut Vec<Expr<'ast, ast::Type<'ast>>>,
    ) -> Result<(), Self::Error> {
        args.retain(|arg| arg.type_() != &ast::Type::Unit);
        Ok(())
    }

    fn visit_type(&mut self, type_: &mut ast::Type<'ast>) -> Result<(), Self::Error> {
        if let ast::Type::Function(ft) = type_ {
            ft.args.retain(|a| a != &ast::Type::Unit);
        }
        Ok(())
    }

    fn post_visit_fun_decl(
        &mut self,
        _name: &mut Ident<'ast>,
        _type_args: &mut Vec<Ident>,
        args: &mut Vec<(Ident, ast::Type<'ast>)>,
        _body: &mut Box<Expr<ast::Type<'ast>>>,
        _type_: &mut ast::Type<'ast>,
    ) -> Result<(), Self::Error> {
        args.retain(|(_, ty)| ty != &ast::Type::Unit);
        Ok(())
    }
}

pub(crate) fn run_toplevel<'a>(toplevel: &mut Vec<Decl<'a, ast::Type<'a>>>) {
    let mut pass = StripPositiveUnits {};
    for decl in toplevel.iter_mut() {
        pass.visit_decl(decl).void_unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::toplevel;
    use crate::tc::typecheck_toplevel;
    use pretty_assertions::assert_eq;

    #[test]
    fn unit_only_arg() {
        let (_, program) = toplevel(
            "ty f : fn () -> int
             fn f _ = 1

             ty main : fn -> int
             fn main = f ()",
        )
        .unwrap();

        let (_, expected) = toplevel(
            "ty f : fn -> int
             fn f = 1

             ty main : fn -> int
             fn main = f()",
        )
        .unwrap();
        let expected = typecheck_toplevel(expected).unwrap();

        let mut program = typecheck_toplevel(program).unwrap();
        run_toplevel(&mut program);

        assert_eq!(program, expected);
    }

    #[test]
    fn unit_and_other_arg() {
        let (_, program) = toplevel(
            "ty f : fn (), int -> int
             fn f _ x = x

             ty main : fn -> int
             fn main = f () 1",
        )
        .unwrap();

        let (_, expected) = toplevel(
            "ty f : fn int -> int
             fn f x = x

             ty main : fn -> int
             fn main = f 1",
        )
        .unwrap();
        let expected = typecheck_toplevel(expected).unwrap();

        let mut program = typecheck_toplevel(program).unwrap();
        run_toplevel(&mut program);

        assert_eq!(program, expected);
    }

    #[test]
    fn unit_expr_and_other_arg() {
        let (_, program) = toplevel(
            "ty f : fn (), int -> int
             fn f _ x = x

             ty g : fn int -> ()
             fn g _ = ()

             ty main : fn -> int
             fn main = f (g 2) 1",
        )
        .unwrap();

        let (_, expected) = toplevel(
            "ty f : fn int -> int
             fn f x = x

             ty g : fn int -> ()
             fn g _ = ()

             ty main : fn -> int
             fn main = let ___discarded = g 2 in f 1",
        )
        .unwrap();
        assert_eq!(expected.len(), 6);
        let expected = typecheck_toplevel(expected).unwrap();

        let mut program = typecheck_toplevel(program).unwrap();
        run_toplevel(&mut program);

        assert_eq!(program, expected);
    }
}
