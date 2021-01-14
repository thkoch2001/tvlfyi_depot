use crate::errors::{Error, ErrorKind};
use crate::parser::{self, Block, Expr, Literal, Statement};
use crate::scanner::{self, TokenKind};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::RwLock;

// Implementation of built-in functions.
mod builtins;

#[cfg(test)]
mod tests;

// Tree-walk interpreter

// Representation of all callables, including builtins & user-defined
// functions.
#[derive(Clone, Debug)]
pub enum Callable<'a> {
    Builtin(&'static dyn builtins::Builtin),
    Function(Rc<parser::Function<'a>>),
}

impl<'a> Callable<'a> {
    fn arity(&self) -> usize {
        match self {
            Callable::Builtin(builtin) => builtin.arity(),
            Callable::Function(func) => func.params.len(),
        }
    }

    fn call(&self, lox: &mut Interpreter<'a>, args: Vec<Value<'a>>) -> Result<Value<'a>, Error> {
        match self {
            Callable::Builtin(builtin) => builtin.call(args),

            Callable::Function(func) => {
                let mut fn_env: Environment<'a> = Default::default();

                for (param, value) in func.params.iter().zip(args.into_iter()) {
                    fn_env.define(param, value)?;
                }

                lox.interpret_block(Rc::new(RwLock::new(fn_env)), &func.body)
            },
        }
    }
}

// Representation of an in-language value.
#[derive(Clone, Debug)]
pub enum Value<'a> {
    Literal(Literal),
    Callable(Callable<'a>),
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Literal(lhs), Value::Literal(rhs)) => lhs == rhs,
            // functions do not have equality
            _ => false,
        }
    }
}

impl<'a> From<Literal> for Value<'a> {
    fn from(lit: Literal) -> Value<'a> {
        Value::Literal(lit)
    }
}

impl<'a> Value<'a> {
    fn expect_literal(self) -> Result<Literal, Error> {
        match self {
            Value::Literal(lit) => Ok(lit),
            _ => unimplemented!(), // which error? which line?
        }
    }
}

#[derive(Debug, Default)]
struct Environment<'a> {
    enclosing: Option<Rc<RwLock<Environment<'a>>>>,
    values: HashMap<String, Value<'a>>,
}

impl<'a> Environment<'a> {
    fn define(&mut self, name: &scanner::Token, value: Value<'a>) -> Result<(), Error> {
        let ident = identifier_str(name)?;
        self.values.insert(ident.into(), value);
        Ok(())
    }

    fn get(&self, name: &parser::Variable) -> Result<Value<'a>, Error> {
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

    fn assign(&mut self, name: &scanner::Token, value: Value<'a>) -> Result<(), Error> {
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

#[derive(Debug)]
pub struct Interpreter<'a> {
    env: Rc<RwLock<Environment<'a>>>,
}

impl<'a> Interpreter<'a> {
    /// Create a new interpreter and configure the initial global
    /// variable set.
    pub fn create() -> Self {
        let mut globals = HashMap::new();

        globals.insert(
            "clock".into(),
            Value::Callable(Callable::Builtin(&builtins::Clock {})),
        );

        Interpreter {
            env: Rc::new(RwLock::new(Environment {
                enclosing: None,
                values: globals,
            })),
        }
    }

    // Environment modification helpers
    fn define_var(&mut self, name: &scanner::Token, value: Value<'a>) -> Result<(), Error> {
        self.env
            .write()
            .expect("environment lock is poisoned")
            .define(name, value)
    }

    fn assign_var(&mut self, name: &scanner::Token, value: Value<'a>) -> Result<(), Error> {
        self.env
            .write()
            .expect("environment lock is poisoned")
            .assign(name, value)
    }

    fn get_var(&mut self, var: &parser::Variable) -> Result<Value<'a>, Error> {
        self.env
            .read()
            .expect("environment lock is poisoned")
            .get(var)
    }

    fn set_enclosing(&mut self, parent: Rc<RwLock<Environment<'a>>>) {
        self.env
            .write()
            .expect("environment lock is poisoned")
            .enclosing = Some(parent);
    }

    // Interpreter itself
    pub fn interpret(&mut self, program: &Block<'a>) -> Result<Value<'a>, Error> {
        let mut value = Value::Literal(Literal::Nil);

        for stmt in program {
            value = self.interpret_stmt(stmt)?;
        }

        Ok(value)
    }

    fn interpret_stmt(&mut self, stmt: &Statement<'a>) -> Result<Value<'a>, Error> {
        let value = match stmt {
            Statement::Expr(expr) => self.eval(expr)?,
            Statement::Print(expr) => {
                let result = self.eval(expr)?;
                let output = format!("{:?}", result);
                println!("{}", output);
                Value::Literal(Literal::String(output))
            }
            Statement::Var(var) => return self.interpret_var(var),
            Statement::Block(block) => return self.interpret_block(Default::default(), block),
            Statement::If(if_stmt) => return self.interpret_if(if_stmt),
            Statement::While(while_stmt) => return self.interpret_while(while_stmt),
            Statement::Function(_) => unimplemented!(),
        };

        Ok(value)
    }

    fn interpret_var(&mut self, var: &parser::Var<'a>) -> Result<Value<'a>, Error> {
        let init = var.initialiser.as_ref().ok_or_else(|| Error {
            line: var.name.line,
            kind: ErrorKind::InternalError("missing variable initialiser".into()),
        })?;
        let value = self.eval(init)?;
        self.define_var(&var.name, value.clone())?;
        Ok(value)
    }

    fn interpret_block(
        &mut self,
        env: Rc<RwLock<Environment<'a>>>,
        block: &parser::Block<'a>,
    ) -> Result<Value<'a>, Error> {
        // Initialise a new environment and point it at the parent
        // (this is a bit tedious because we need to wrap it in and
        // out of the Rc).
        //
        // TODO(tazjin): Refactor this to use Rc on the interpreter itself.
        let previous = std::mem::replace(&mut self.env, env);
        self.set_enclosing(previous.clone());

        let result = self.interpret(block);

        // Swap it back, discarding the child env.
        self.env = previous;

        return result;
    }

    fn interpret_if(&mut self, if_stmt: &parser::If<'a>) -> Result<Value<'a>, Error> {
        let condition = self.eval(&if_stmt.condition)?;

        if eval_truthy(&condition) {
            self.interpret_stmt(&if_stmt.then_branch)
        } else if let Some(else_branch) = &if_stmt.else_branch {
            self.interpret_stmt(else_branch)
        } else {
            Ok(Value::Literal(Literal::Nil))
        }
    }

    fn interpret_while(&mut self, stmt: &parser::While<'a>) -> Result<Value<'a>, Error> {
        let mut value = Value::Literal(Literal::Nil);
        while eval_truthy(&self.eval(&stmt.condition)?) {
            value = self.interpret_stmt(&stmt.body)?;
        }

        Ok(value)
    }

    fn eval(&mut self, expr: &Expr<'a>) -> Result<Value<'a>, Error> {
        match expr {
            Expr::Assign(assign) => self.eval_assign(assign),
            Expr::Literal(lit) => Ok(lit.clone().into()),
            Expr::Grouping(grouping) => self.eval(&*grouping.0),
            Expr::Unary(unary) => self.eval_unary(unary),
            Expr::Binary(binary) => self.eval_binary(binary),
            Expr::Variable(var) => self.get_var(var),
            Expr::Logical(log) => self.eval_logical(log),
            Expr::Call(call) => self.eval_call(call),
        }
    }

    fn eval_unary(&mut self, expr: &parser::Unary<'a>) -> Result<Value<'a>, Error> {
        let right = self.eval(&*expr.right)?;

        match (&expr.operator.kind, right) {
            (TokenKind::Minus, Value::Literal(Literal::Number(num))) => {
                Ok(Literal::Number(-num).into())
            }
            (TokenKind::Bang, right) => Ok(Literal::Boolean(!eval_truthy(&right)).into()),

            (op, right) => Err(Error {
                line: expr.operator.line,
                kind: ErrorKind::TypeError(format!(
                    "Operator '{:?}' can not be called with argument '{:?}'",
                    op, right
                )),
            }),
        }
    }

    fn eval_binary(&mut self, expr: &parser::Binary<'a>) -> Result<Value<'a>, Error> {
        let left = self.eval(&*expr.left)?.expect_literal()?;
        let right = self.eval(&*expr.right)?.expect_literal()?;

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

        Ok(result.into())
    }

    fn eval_assign(&mut self, assign: &parser::Assign<'a>) -> Result<Value<'a>, Error> {
        let value = self.eval(&assign.value)?;
        self.assign_var(&assign.name, value.clone())?;
        Ok(value)
    }

    fn eval_logical(&mut self, logical: &parser::Logical<'a>) -> Result<Value<'a>, Error> {
        let left = eval_truthy(&self.eval(&logical.left)?);
        let right = eval_truthy(&self.eval(&logical.right)?);

        match &logical.operator.kind {
            TokenKind::And => Ok(Literal::Boolean(left && right).into()),
            TokenKind::Or => Ok(Literal::Boolean(left || right).into()),
            kind => Err(Error {
                line: logical.operator.line,
                kind: ErrorKind::InternalError(format!("Invalid logical operator: {:?}", kind)),
            }),
        }
    }

    fn eval_call(&mut self, call: &parser::Call<'a>) -> Result<Value<'a>, Error> {
        let callable = match self.eval(&call.callee)? {
            Value::Callable(c) => c,
            Value::Literal(v) => {
                return Err(Error {
                    line: call.paren.line,
                    kind: ErrorKind::RuntimeError(format!("not callable: {:?}", v)),
                })
            }
        };

        let mut args = vec![];
        for arg in &call.args {
            args.push(self.eval(arg)?);
        }

        if callable.arity() != args.len() {
            return Err(Error {
                line: call.paren.line,
                kind: ErrorKind::RuntimeError(format!(
                    "Expected {} arguments, but got {}",
                    callable.arity(),
                    args.len(),
                )),
            });
        }

        callable.call(self, args)
    }
}

// Interpreter functions not dependent on interpreter-state.

fn eval_truthy(lit: &Value) -> bool {
    if let Value::Literal(lit) = lit {
        match lit {
            Literal::Nil => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    } else {
        false
    }
}
