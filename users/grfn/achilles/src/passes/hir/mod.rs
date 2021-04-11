use std::collections::HashMap;

use crate::ast::hir::{Binding, Decl, Expr};
use crate::ast::{BinaryOperator, Ident, Literal, UnaryOperator};

pub(crate) mod monomorphize;
pub(crate) mod strip_positive_units;

pub(crate) trait Visitor<'a, 'ast, T: 'ast>: Sized + 'a {
    type Error;

    fn visit_type(&mut self, _type: &mut T) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_ident(&mut self, _ident: &mut Ident<'ast>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_literal(&mut self, _literal: &mut Literal<'ast>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_unary_operator(&mut self, _op: &mut UnaryOperator) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_binary_operator(&mut self, _op: &mut BinaryOperator) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_binding(&mut self, binding: &mut Binding<'ast, T>) -> Result<(), Self::Error> {
        self.visit_ident(&mut binding.ident)?;
        self.visit_type(&mut binding.type_)?;
        self.visit_expr(&mut binding.body)?;
        Ok(())
    }

    fn post_visit_call(
        &mut self,
        _fun: &mut Expr<'ast, T>,
        _type_args: &mut HashMap<Ident<'ast>, T>,
        _args: &mut Vec<Expr<'ast, T>>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pre_visit_call(
        &mut self,
        _fun: &mut Expr<'ast, T>,
        _type_args: &mut HashMap<Ident<'ast>, T>,
        _args: &mut Vec<Expr<'ast, T>>,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pre_visit_expr(&mut self, _expr: &mut Expr<'ast, T>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_expr(&mut self, expr: &mut Expr<'ast, T>) -> Result<(), Self::Error> {
        self.pre_visit_expr(expr)?;
        match expr {
            Expr::Ident(id, t) => {
                self.visit_ident(id)?;
                self.visit_type(t)?;
            }
            Expr::Literal(lit, t) => {
                self.visit_literal(lit)?;
                self.visit_type(t)?;
            }
            Expr::UnaryOp { op, rhs, type_ } => {
                self.visit_unary_operator(op)?;
                self.visit_expr(rhs)?;
                self.visit_type(type_)?;
            }
            Expr::BinaryOp {
                lhs,
                op,
                rhs,
                type_,
            } => {
                self.visit_expr(lhs)?;
                self.visit_binary_operator(op)?;
                self.visit_expr(rhs)?;
                self.visit_type(type_)?;
            }
            Expr::Let {
                bindings,
                body,
                type_,
            } => {
                for binding in bindings.iter_mut() {
                    self.visit_binding(binding)?;
                }
                self.visit_expr(body)?;
                self.visit_type(type_)?;
            }
            Expr::If {
                condition,
                then,
                else_,
                type_,
            } => {
                self.visit_expr(condition)?;
                self.visit_expr(then)?;
                self.visit_expr(else_)?;
                self.visit_type(type_)?;
            }
            Expr::Fun {
                args,
                body,
                type_args,
                type_,
            } => {
                for (ident, t) in args {
                    self.visit_ident(ident)?;
                    self.visit_type(t)?;
                }
                for ta in type_args {
                    self.visit_ident(ta)?;
                }
                self.visit_expr(body)?;
                self.visit_type(type_)?;
            }
            Expr::Call {
                fun,
                args,
                type_args,
                type_,
            } => {
                self.pre_visit_call(fun, type_args, args)?;
                self.visit_expr(fun)?;
                for arg in args.iter_mut() {
                    self.visit_expr(arg)?;
                }
                self.visit_type(type_)?;
                self.post_visit_call(fun, type_args, args)?;
            }
        }

        Ok(())
    }

    fn post_visit_decl(&mut self, decl: &'a Decl<'ast, T>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn post_visit_fun_decl(
        &mut self,
        _name: &mut Ident<'ast>,
        _type_args: &mut Vec<Ident>,
        _args: &mut Vec<(Ident, T)>,
        _body: &mut Box<Expr<T>>,
        _type_: &mut T,
    ) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_decl(&mut self, decl: &'a mut Decl<'ast, T>) -> Result<(), Self::Error> {
        match decl {
            Decl::Fun {
                name,
                type_args,
                args,
                body,
                type_,
            } => {
                self.visit_ident(name)?;
                for type_arg in type_args.iter_mut() {
                    self.visit_ident(type_arg)?;
                }
                for (arg, t) in args.iter_mut() {
                    self.visit_ident(arg)?;
                    self.visit_type(t)?;
                }
                self.visit_expr(body)?;
                self.visit_type(type_)?;
                self.post_visit_fun_decl(name, type_args, args, body, type_)?;
            }
            Decl::Extern {
                name,
                arg_types,
                ret_type,
            } => {
                self.visit_ident(name)?;
                for arg_t in arg_types {
                    self.visit_type(arg_t)?;
                }
                self.visit_type(ret_type)?;
            }
        }

        self.post_visit_decl(decl)?;
        Ok(())
    }
}
