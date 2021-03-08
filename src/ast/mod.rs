use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};

use itertools::Itertools;

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidIdentifier<'a>(Cow<'a, str>);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ident<'a>(pub Cow<'a, str>);

impl<'a> From<&'a Ident<'a>> for &'a str {
    fn from(id: &'a Ident<'a>) -> Self {
        id.0.as_ref()
    }
}

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Ident<'a> {
    pub fn to_owned(&self) -> Ident<'static> {
        Ident(Cow::Owned(self.0.clone().into_owned()))
    }

    pub fn from_str_unchecked(s: &'a str) -> Self {
        debug_assert!(is_valid_identifier(s));
        Self(Cow::Borrowed(s))
    }

    pub fn from_string_unchecked(s: String) -> Self {
        debug_assert!(is_valid_identifier(&s));
        Self(Cow::Owned(s))
    }
}

pub fn is_valid_identifier<S>(s: &S) -> bool
where
    S: AsRef<str> + ?Sized,
{
    s.as_ref()
        .chars()
        .any(|c| !c.is_alphanumeric() || !"_".contains(c))
}

impl<'a> TryFrom<&'a str> for Ident<'a> {
    type Error = InvalidIdentifier<'a>;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        if is_valid_identifier(s) {
            Ok(Ident(Cow::Borrowed(s)))
        } else {
            Err(InvalidIdentifier(Cow::Borrowed(s)))
        }
    }
}

impl<'a> TryFrom<String> for Ident<'a> {
    type Error = InvalidIdentifier<'static>;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        if is_valid_identifier(&s) {
            Ok(Ident(Cow::Owned(s)))
        } else {
            Err(InvalidIdentifier(Cow::Owned(s)))
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOperator {
    /// `+`
    Add,

    /// `-`
    Sub,

    /// `*`
    Mul,

    /// `/`
    Div,

    /// `^`
    Pow,

    /// `==`
    Equ,

    /// `!=`
    Neq,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnaryOperator {
    /// !
    Not,

    /// -
    Neg,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal {
    Int(u64),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr<'a> {
    Ident(Ident<'a>),

    Literal(Literal),

    UnaryOp {
        op: UnaryOperator,
        rhs: Box<Expr<'a>>,
    },

    BinaryOp {
        lhs: Box<Expr<'a>>,
        op: BinaryOperator,
        rhs: Box<Expr<'a>>,
    },

    Let {
        bindings: Vec<(Ident<'a>, Expr<'a>)>,
        body: Box<Expr<'a>>,
    },

    If {
        condition: Box<Expr<'a>>,
        then: Box<Expr<'a>>,
        else_: Box<Expr<'a>>,
    },

    Fun(Box<Fun<'a>>),

    Call {
        fun: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
}

impl<'a> Expr<'a> {
    pub fn to_owned(&self) -> Expr<'static> {
        match self {
            Expr::Ident(ref id) => Expr::Ident(id.to_owned()),
            Expr::Literal(ref lit) => Expr::Literal(lit.clone()),
            Expr::UnaryOp { op, rhs } => Expr::UnaryOp {
                op: *op,
                rhs: Box::new((**rhs).to_owned()),
            },
            Expr::BinaryOp { lhs, op, rhs } => Expr::BinaryOp {
                lhs: Box::new((**lhs).to_owned()),
                op: *op,
                rhs: Box::new((**rhs).to_owned()),
            },
            Expr::Let { bindings, body } => Expr::Let {
                bindings: bindings
                    .iter()
                    .map(|(id, expr)| (id.to_owned(), expr.to_owned()))
                    .collect(),
                body: Box::new((**body).to_owned()),
            },
            Expr::If {
                condition,
                then,
                else_,
            } => Expr::If {
                condition: Box::new((**condition).to_owned()),
                then: Box::new((**then).to_owned()),
                else_: Box::new((**else_).to_owned()),
            },
            Expr::Fun(fun) => Expr::Fun(Box::new((**fun).to_owned())),
            Expr::Call { fun, args } => Expr::Call {
                fun: Box::new((**fun).to_owned()),
                args: args.iter().map(|arg| arg.to_owned()).collect(),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fun<'a> {
    pub args: Vec<Ident<'a>>,
    pub body: Expr<'a>,
}

impl<'a> Fun<'a> {
    fn to_owned(&self) -> Fun<'static> {
        Fun {
            args: self.args.iter().map(|arg| arg.to_owned()).collect(),
            body: self.body.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decl<'a> {
    Fun { name: Ident<'a>, body: Fun<'a> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn {} -> {}", self.args.iter().join(", "), self.ret)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Function(FunctionType),
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => f.write_str("int"),
            Type::Float => f.write_str("float"),
            Type::Bool => f.write_str("bool"),
            Type::Function(ft) => ft.fmt(f),
        }
    }
}
