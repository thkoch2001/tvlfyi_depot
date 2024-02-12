use std::collections::HashMap;

use itertools::Itertools;

use super::{BinaryOperator, Ident, Literal, UnaryOperator};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern<'a, T> {
    Id(Ident<'a>, T),
    Tuple(Vec<Pattern<'a, T>>),
}

impl<'a, T> Pattern<'a, T> {
    pub fn to_owned(&self) -> Pattern<'static, T>
    where
        T: Clone,
    {
        match self {
            Pattern::Id(id, t) => Pattern::Id(id.to_owned(), t.clone()),
            Pattern::Tuple(pats) => {
                Pattern::Tuple(pats.into_iter().map(Pattern::to_owned).collect())
            }
        }
    }

    pub fn traverse_type<F, U, E>(self, f: F) -> Result<Pattern<'a, U>, E>
    where
        F: Fn(T) -> Result<U, E> + Clone,
    {
        match self {
            Pattern::Id(id, t) => Ok(Pattern::Id(id, f(t)?)),
            Pattern::Tuple(pats) => Ok(Pattern::Tuple(
                pats.into_iter()
                    .map(|pat| pat.traverse_type(f.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Binding<'a, T> {
    pub pat: Pattern<'a, T>,
    pub body: Expr<'a, T>,
}

impl<'a, T> Binding<'a, T> {
    fn to_owned(&self) -> Binding<'static, T>
    where
        T: Clone,
    {
        Binding {
            pat: self.pat.to_owned(),
            body: self.body.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<'a, T> {
    Ident(Ident<'a>, T),

    Literal(Literal<'a>, T),

    Tuple(Vec<Expr<'a, T>>, T),

    UnaryOp {
        op: UnaryOperator,
        rhs: Box<Expr<'a, T>>,
        type_: T,
    },

    BinaryOp {
        lhs: Box<Expr<'a, T>>,
        op: BinaryOperator,
        rhs: Box<Expr<'a, T>>,
        type_: T,
    },

    Let {
        bindings: Vec<Binding<'a, T>>,
        body: Box<Expr<'a, T>>,
        type_: T,
    },

    If {
        condition: Box<Expr<'a, T>>,
        then: Box<Expr<'a, T>>,
        else_: Box<Expr<'a, T>>,
        type_: T,
    },

    Fun {
        type_args: Vec<Ident<'a>>,
        args: Vec<(Ident<'a>, T)>,
        body: Box<Expr<'a, T>>,
        type_: T,
    },

    Call {
        fun: Box<Expr<'a, T>>,
        type_args: HashMap<Ident<'a>, T>,
        args: Vec<Expr<'a, T>>,
        type_: T,
    },
}

impl<'a, T> Expr<'a, T> {
    pub fn type_(&self) -> &T {
        match self {
            Expr::Ident(_, t) => t,
            Expr::Literal(_, t) => t,
            Expr::Tuple(_, t) => t,
            Expr::UnaryOp { type_, .. } => type_,
            Expr::BinaryOp { type_, .. } => type_,
            Expr::Let { type_, .. } => type_,
            Expr::If { type_, .. } => type_,
            Expr::Fun { type_, .. } => type_,
            Expr::Call { type_, .. } => type_,
        }
    }

    pub fn traverse_type<F, U, E>(self, f: F) -> Result<Expr<'a, U>, E>
    where
        F: Fn(T) -> Result<U, E> + Clone,
    {
        match self {
            Expr::Ident(id, t) => Ok(Expr::Ident(id, f(t)?)),
            Expr::Literal(lit, t) => Ok(Expr::Literal(lit, f(t)?)),
            Expr::UnaryOp { op, rhs, type_ } => Ok(Expr::UnaryOp {
                op,
                rhs: Box::new(rhs.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Expr::BinaryOp {
                lhs,
                op,
                rhs,
                type_,
            } => Ok(Expr::BinaryOp {
                lhs: Box::new(lhs.traverse_type(f.clone())?),
                op,
                rhs: Box::new(rhs.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Expr::Let {
                bindings,
                body,
                type_,
            } => Ok(Expr::Let {
                bindings: bindings
                    .into_iter()
                    .map(|Binding { pat, body }| {
                        Ok(Binding {
                            pat: pat.traverse_type(f.clone())?,
                            body: body.traverse_type(f.clone())?,
                        })
                    })
                    .collect::<Result<Vec<_>, E>>()?,
                body: Box::new(body.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Expr::If {
                condition,
                then,
                else_,
                type_,
            } => Ok(Expr::If {
                condition: Box::new(condition.traverse_type(f.clone())?),
                then: Box::new(then.traverse_type(f.clone())?),
                else_: Box::new(else_.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Expr::Fun {
                args,
                type_args,
                body,
                type_,
            } => Ok(Expr::Fun {
                args: args
                    .into_iter()
                    .map(|(id, t)| Ok((id, f.clone()(t)?)))
                    .collect::<Result<Vec<_>, E>>()?,
                type_args,
                body: Box::new(body.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Expr::Call {
                fun,
                type_args,
                args,
                type_,
            } => Ok(Expr::Call {
                fun: Box::new(fun.traverse_type(f.clone())?),
                type_args: type_args
                    .into_iter()
                    .map(|(id, ty)| Ok((id, f.clone()(ty)?)))
                    .collect::<Result<HashMap<_, _>, E>>()?,
                args: args
                    .into_iter()
                    .map(|e| e.traverse_type(f.clone()))
                    .collect::<Result<Vec<_>, E>>()?,
                type_: f(type_)?,
            }),
            Expr::Tuple(members, t) => Ok(Expr::Tuple(
                members
                    .into_iter()
                    .map(|t| t.traverse_type(f.clone()))
                    .try_collect()?,
                f(t)?,
            )),
        }
    }

    pub fn to_owned(&self) -> Expr<'static, T>
    where
        T: Clone,
    {
        match self {
            Expr::Ident(id, t) => Expr::Ident(id.to_owned(), t.clone()),
            Expr::Literal(lit, t) => Expr::Literal(lit.to_owned(), t.clone()),
            Expr::UnaryOp { op, rhs, type_ } => Expr::UnaryOp {
                op: *op,
                rhs: Box::new((**rhs).to_owned()),
                type_: type_.clone(),
            },
            Expr::BinaryOp {
                lhs,
                op,
                rhs,
                type_,
            } => Expr::BinaryOp {
                lhs: Box::new((**lhs).to_owned()),
                op: *op,
                rhs: Box::new((**rhs).to_owned()),
                type_: type_.clone(),
            },
            Expr::Let {
                bindings,
                body,
                type_,
            } => Expr::Let {
                bindings: bindings.iter().map(|b| b.to_owned()).collect(),
                body: Box::new((**body).to_owned()),
                type_: type_.clone(),
            },
            Expr::If {
                condition,
                then,
                else_,
                type_,
            } => Expr::If {
                condition: Box::new((**condition).to_owned()),
                then: Box::new((**then).to_owned()),
                else_: Box::new((**else_).to_owned()),
                type_: type_.clone(),
            },
            Expr::Fun {
                args,
                type_args,
                body,
                type_,
            } => Expr::Fun {
                args: args
                    .iter()
                    .map(|(id, t)| (id.to_owned(), t.clone()))
                    .collect(),
                type_args: type_args.iter().map(|arg| arg.to_owned()).collect(),
                body: Box::new((**body).to_owned()),
                type_: type_.clone(),
            },
            Expr::Call {
                fun,
                type_args,
                args,
                type_,
            } => Expr::Call {
                fun: Box::new((**fun).to_owned()),
                type_args: type_args
                    .iter()
                    .map(|(id, t)| (id.to_owned(), t.clone()))
                    .collect(),
                args: args.iter().map(|e| e.to_owned()).collect(),
                type_: type_.clone(),
            },
            Expr::Tuple(members, t) => {
                Expr::Tuple(members.into_iter().map(Expr::to_owned).collect(), t.clone())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl<'a, T> {
    Fun {
        name: Ident<'a>,
        type_args: Vec<Ident<'a>>,
        args: Vec<(Ident<'a>, T)>,
        body: Box<Expr<'a, T>>,
        type_: T,
    },

    Extern {
        name: Ident<'a>,
        arg_types: Vec<T>,
        ret_type: T,
    },
}

impl<'a, T> Decl<'a, T> {
    pub fn name(&self) -> &Ident<'a> {
        match self {
            Decl::Fun { name, .. } => name,
            Decl::Extern { name, .. } => name,
        }
    }

    pub fn set_name(&mut self, new_name: Ident<'a>) {
        match self {
            Decl::Fun { name, .. } => *name = new_name,
            Decl::Extern { name, .. } => *name = new_name,
        }
    }

    pub fn type_(&self) -> Option<&T> {
        match self {
            Decl::Fun { type_, .. } => Some(type_),
            Decl::Extern { .. } => None,
        }
    }

    pub fn traverse_type<F, U, E>(self, f: F) -> Result<Decl<'a, U>, E>
    where
        F: Fn(T) -> Result<U, E> + Clone,
    {
        match self {
            Decl::Fun {
                name,
                type_args,
                args,
                body,
                type_,
            } => Ok(Decl::Fun {
                name,
                type_args,
                args: args
                    .into_iter()
                    .map(|(id, t)| Ok((id, f(t)?)))
                    .try_collect()?,
                body: Box::new(body.traverse_type(f.clone())?),
                type_: f(type_)?,
            }),
            Decl::Extern {
                name,
                arg_types,
                ret_type,
            } => Ok(Decl::Extern {
                name,
                arg_types: arg_types.into_iter().map(f.clone()).try_collect()?,
                ret_type: f(ret_type)?,
            }),
        }
    }
}
