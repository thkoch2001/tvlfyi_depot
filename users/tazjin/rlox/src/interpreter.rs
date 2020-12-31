use crate::errors::{Error, ErrorKind};
use crate::parser::{self, Declaration, Expr, Literal, Program, Statement};
use crate::scanner::{self, TokenKind};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

// Tree-walk interpreter

#[derive(Debug, Default)]
struct Environment {
    enclosing: Option<Rc<RwLock<Environment>>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    fn define(&mut self, name: &scanner::Token, value: Literal) -> Result<(), Error> {
        let ident = identifier_str(name)?;
        self.values.insert(ident.into(), value);
        Ok(())
    }

    fn get(&self, name: &parser::Variable) -> Result<Literal, Error> {
        let ident = identifier_str(&name.0)?;

        self.values
            .get(ident)
            .map(Clone::clone)
            .ok_or_else(|| Error {
                line: name.0.line,
                kind: ErrorKind::UndefinedVariable(ident.into()),
            })
            .or_else(|err| {
                if let Some(parent) = &self.enclosing {
                    parent.read().unwrap().get(name)
                } else {
                    Err(err)
                }
            })
    }

    fn assign(&mut self, name: &scanner::Token, value: Literal) -> Result<(), Error> {
        let ident = identifier_str(name)?;

        match self.values.get_mut(ident) {
            Some(target) => {
                *target = value;
                Ok(())
            }
            None => {
                if let Some(parent) = &self.enclosing {
                    return parent.write().unwrap().assign(name, value);
                }

                Err(Error {
                    line: name.line,
                    kind: ErrorKind::UndefinedVariable(ident.into()),
                })
            }
        }
    }
}

fn identifier_str<'a>(name: &'a scanner::Token) -> Result<&'a str, Error> {
    if let TokenKind::Identifier(ident) = &name.kind {
        Ok(ident)
    } else {
        Err(Error {
            line: name.line,
            kind: ErrorKind::InternalError("unexpected identifier kind".into()),
        })
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    globals: Environment,
}

impl Interpreter {
    fn interpret_stmt<'a>(&mut self, stmt: &Statement<'a>) -> Result<(), Error> {
        match stmt {
            Statement::Expr(expr) => {
                self.eval(expr)?;
            }
            Statement::Print(expr) => {
                let result = self.eval(expr)?;
                println!("{:?}", result)
            }
        }

        Ok(())
    }

    fn interpret_var<'a>(&mut self, var: &parser::Var<'a>) -> Result<(), Error> {
        let init = var.initialiser.as_ref().ok_or_else(|| Error {
            line: var.name.line,
            kind: ErrorKind::InternalError("missing variable initialiser".into()),
        })?;
        let value = self.eval(init)?;
        self.globals.define(&var.name, value)?;
        return Ok(());
    }

    pub fn interpret<'a>(&mut self, program: &Program<'a>) -> Result<(), Error> {
        for decl in program {
            match decl {
                Declaration::Stmt(stmt) => self.interpret_stmt(stmt)?,
                Declaration::Var(var) => self.interpret_var(var)?,
            }
        }

        Ok(())
    }

    fn eval<'a>(&mut self, expr: &Expr<'a>) -> Result<Literal, Error> {
        match expr {
            Expr::Assign(assign) => self.eval_assign(assign),
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Grouping(grouping) => self.eval(&*grouping.0),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Variable(var) => self.globals.get(var),
        }
    }

    fn eval_unary<'a>(&mut self, expr: &parser::Unary<'a>) -> Result<Literal, Error> {
        let right = self.eval(&*expr.right)?;

        match (&expr.operator.kind, right) {
            (TokenKind::Minus, Literal::Number(num)) => Ok(Literal::Number(-num)),
            (TokenKind::Bang, right) => Ok(Literal::Boolean(!eval_truthy(&right))),

            (op, right) => Err(Error {
                line: expr.operator.line,
                kind: ErrorKind::TypeError(format!(
                    "Operator '{:?}' can not be called with argument '{:?}'",
                    op, right
                )),
            }),
        }
    }

    fn eval_binary<'a>(&mut self, expr: &parser::Binary<'a>) -> Result<Literal, Error> {
        let left = self.eval(&*expr.left)?;
        let right = self.eval(&*expr.right)?;

        let result = match (&expr.operator.kind, left, right) {
            // Numeric
            (TokenKind::Minus, Literal::Number(l), Literal::Number(r)) => Literal::Number(l - r),
            (TokenKind::Slash, Literal::Number(l), Literal::Number(r)) => Literal::Number(l / r),
            (TokenKind::Star, Literal::Number(l), Literal::Number(r)) => Literal::Number(l * r),
            (TokenKind::Plus, Literal::Number(l), Literal::Number(r)) => Literal::Number(l + r),

            // Strings
            (TokenKind::Plus, Literal::String(l), Literal::String(r)) => {
                Literal::String(format!("{}{}", l, r))
            }

            // Comparators (on numbers only?)
            (TokenKind::Greater, Literal::Number(l), Literal::Number(r)) => Literal::Boolean(l > r),
            (TokenKind::GreaterEqual, Literal::Number(l), Literal::Number(r)) => {
                Literal::Boolean(l >= r)
            }
            (TokenKind::Less, Literal::Number(l), Literal::Number(r)) => Literal::Boolean(l < r),
            (TokenKind::LessEqual, Literal::Number(l), Literal::Number(r)) => {
                Literal::Boolean(l <= r)
            }

            // Equality
            (TokenKind::Equal, l, r) => Literal::Boolean(l == r),
            (TokenKind::BangEqual, l, r) => Literal::Boolean(l != r),

            (op, left, right) => {
                return Err(Error {
                    line: expr.operator.line,
                    kind: ErrorKind::TypeError(format!(
                        "Operator '{:?}' can not be called with arguments '({:?}, {:?})'",
                        op, left, right
                    )),
                })
            }
        };

        Ok(result)
    }

    fn eval_assign<'a>(&mut self, assign: &parser::Assign<'a>) -> Result<Literal, Error> {
        let value = self.eval(&assign.value)?;
        self.globals.assign(&assign.name, value.clone())?;
        Ok(value)
    }
}

// Interpreter functions not dependent on interpreter-state.

fn eval_truthy(lit: &Literal) -> bool {
    match lit {
        Literal::Nil => false,
        Literal::Boolean(b) => *b,
        _ => true,
    }
}
