use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::mem;

use void::{ResultVoidExt, Void};

use crate::ast::hir::{Decl, Expr};
use crate::ast::{self, Ident};

use super::Visitor;

#[derive(Default)]
pub(crate) struct Monomorphize<'a, 'ast> {
    decls: HashMap<&'a Ident<'ast>, &'a Decl<'ast, ast::Type<'ast>>>,
    extra_decls: Vec<Decl<'ast, ast::Type<'ast>>>,
    remove_decls: HashSet<Ident<'ast>>,
}

impl<'a, 'ast> Monomorphize<'a, 'ast> {
    pub(crate) fn new() -> Self {
        Default::default()
    }
}

impl<'a, 'ast> Visitor<'a, 'ast, ast::Type<'ast>> for Monomorphize<'a, 'ast> {
    type Error = Void;

    fn post_visit_call(
        &mut self,
        fun: &mut Expr<'ast, ast::Type<'ast>>,
        type_args: &mut HashMap<Ident<'ast>, ast::Type<'ast>>,
        args: &mut Vec<Expr<'ast, ast::Type<'ast>>>,
    ) -> Result<(), Self::Error> {
        let new_fun = match fun {
            Expr::Ident(id, _) => {
                let decl: Decl<_> = (**self.decls.get(id).unwrap()).clone();
                let name = RefCell::new(id.to_string());
                let type_args = mem::take(type_args);
                let mut monomorphized = decl
                    .traverse_type(|ty| -> Result<_, Void> {
                        Ok(ty.clone().traverse_type_vars(|v| {
                            let concrete = type_args.get(&v).unwrap();
                            name.borrow_mut().push_str(&concrete.to_string());
                            concrete.clone()
                        }))
                    })
                    .void_unwrap();
                let name: Ident = name.into_inner().try_into().unwrap();
                if name != *id {
                    self.remove_decls.insert(id.clone());
                    monomorphized.set_name(name.clone());
                    let type_ = monomorphized.type_().unwrap().clone();
                    self.extra_decls.push(monomorphized);
                    Some(Expr::Ident(name, type_))
                } else {
                    None
                }
            }
            _ => todo!(),
        };
        if let Some(new_fun) = new_fun {
            *fun = new_fun;
        }
        Ok(())
    }

    fn post_visit_decl(
        &mut self,
        decl: &'a Decl<'ast, ast::Type<'ast>>,
    ) -> Result<(), Self::Error> {
        self.decls.insert(decl.name(), decl);
        Ok(())
    }
}

pub(crate) fn run_toplevel<'a>(toplevel: &mut Vec<Decl<'a, ast::Type<'a>>>) {
    let mut pass = Monomorphize::new();
    for decl in toplevel.iter_mut() {
        pass.visit_decl(decl).void_unwrap();
    }
    let remove_decls = mem::take(&mut pass.remove_decls);
    let mut extra_decls = mem::take(&mut pass.extra_decls);
    toplevel.retain(|decl| !remove_decls.contains(decl.name()));
    extra_decls.append(toplevel);
    *toplevel = extra_decls;
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::*;
    use crate::parser::toplevel;
    use crate::tc::typecheck_toplevel;

    #[test]
    fn call_id_decl() {
        let (_, program) = toplevel(
            "ty id : fn a -> a
             fn id x = x

             ty main : fn -> int
             fn main = id 0",
        )
        .unwrap();
        let mut program = typecheck_toplevel(program).unwrap();
        run_toplevel(&mut program);

        let find_decl = |ident: &str| {
            program.iter().find(|decl| {
                matches!(decl, Decl::Fun {name, ..} if name == &Ident::try_from(ident).unwrap())
            }).unwrap()
        };

        let main = find_decl("main");
        let body = match main {
            Decl::Fun { body, .. } => body,
            _ => unreachable!(),
        };

        let expected_type = ast::Type::Function(ast::FunctionType {
            args: vec![ast::Type::Int],
            ret: Box::new(ast::Type::Int),
        });

        match &**body {
            Expr::Call { fun, .. } => {
                let fun = match &**fun {
                    Expr::Ident(fun, _) => fun,
                    _ => unreachable!(),
                };
                let called_decl = find_decl(fun.into());
                assert_eq!(called_decl.type_().unwrap(), &expected_type);
            }
            _ => unreachable!(),
        }
    }
}
