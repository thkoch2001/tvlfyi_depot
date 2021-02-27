// Resolves variable access to their specific instances in the
// environment chain.
//
// https://craftinginterpreters.com/resolving-and-binding.html

use std::collections::HashMap;
use std::rc::Rc;

use crate::treewalk::errors::{Error, ErrorKind};
use crate::treewalk::parser::{self, Expr, Statement};
use crate::treewalk::scanner::Token;

#[derive(Default)]
struct Resolver<'a> {
    scopes: Vec<HashMap<&'a str, bool>>,
}

impl<'a> Resolver<'a> {
    // AST traversal
    fn resolve(&mut self, program: &'a mut parser::Block) -> Result<(), Error> {
        self.begin_scope();
        for stmt in program {
            self.resolve_stmt(stmt)?;
        }
        self.end_scope();

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &'a mut Statement) -> Result<(), Error> {
        match stmt {
            Statement::Expr(expr) => self.resolve_expr(expr),
            Statement::Print(expr) => self.resolve_expr(expr),
            Statement::Var(var) => self.resolve_var(var),
            Statement::Return(ret) => self.resolve_expr(&mut ret.value),
            Statement::Block(block) => self.resolve(block),

            Statement::If(if_stmt) => {
                self.resolve_expr(&mut if_stmt.condition)?;
                self.resolve_stmt(&mut if_stmt.then_branch)?;

                if let Some(branch) = if_stmt.else_branch.as_mut() {
                    self.resolve_stmt(branch)?;
                }

                Ok(())
            }

            Statement::While(while_stmt) => {
                self.resolve_expr(&mut while_stmt.condition)?;
                self.resolve_stmt(&mut while_stmt.body)
            }

            Statement::Function(func) => match Rc::get_mut(func) {
                Some(func) => self.resolve_function(func),
                // The resolver does not clone references, so unless
                // the interpreter is called before the resolver this
                // case should never happen.
                None => return Err(Error {
                    line: 0,
                    kind: ErrorKind::InternalError(
                        "multiple function references before interpretation"
                            .into(),
                    ),
                }),
            },
        }
    }

    fn resolve_var(&mut self, var: &'a mut parser::Var) -> Result<(), Error> {
        self.declare(&var.name.lexeme);

        if let Some(init) = &mut var.initialiser {
            self.resolve_expr(init)?;
        }

        self.define(&var.name.lexeme);

        Ok(())
    }

    fn resolve_function(
        &mut self,
        func: &'a mut parser::Function,
    ) -> Result<(), Error> {
        self.declare(&func.name.lexeme);
        self.define(&func.name.lexeme);

        self.begin_scope();

        for param in &func.params {
            self.declare(&param.lexeme);
            self.define(&param.lexeme);
        }

        for stmt in &mut func.body {
            self.resolve_stmt(stmt)?;
        }

        self.end_scope();

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &'a mut Expr) -> Result<(), Error> {
        match expr {
            Expr::Variable(var) => self.resolve_variable(var),
            Expr::Assign(assign) => self.resolve_assign(assign),
            Expr::Grouping(grouping) => self.resolve_expr(&mut grouping.0),
            Expr::Call(call) => self.resolve_call(call),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(unary) => self.resolve_expr(&mut unary.right),

            Expr::Logical(log) => {
                self.resolve_expr(&mut log.left)?;
                self.resolve_expr(&mut log.right)
            }

            Expr::Binary(binary) => {
                self.resolve_expr(&mut binary.left)?;
                self.resolve_expr(&mut binary.right)
            }
        }
    }

    fn resolve_variable(
        &mut self,
        var: &'a mut parser::Variable,
    ) -> Result<(), Error> {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(false) = scope.get(var.name.lexeme.as_str()) {
                return Err(Error {
                    line: var.name.line,
                    kind: ErrorKind::StaticError(
                        "can't read local variable in its own initialiser"
                            .into(),
                    ),
                });
            }
        }

        var.depth = self.resolve_local(&var.name);
        Ok(())
    }

    fn resolve_assign(
        &mut self,
        assign: &'a mut parser::Assign,
    ) -> Result<(), Error> {
        self.resolve_expr(&mut assign.value)?;
        assign.depth = self.resolve_local(&assign.name);
        Ok(())
    }

    fn resolve_local(&mut self, name: &'a Token) -> Option<usize> {
        for (c, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name.lexeme.as_str()) {
                return Some(c);
            }
        }

        None
    }

    fn resolve_call(
        &mut self,
        call: &'a mut parser::Call,
    ) -> Result<(), Error> {
        self.resolve_expr(&mut call.callee)?;

        for arg in call.args.iter_mut() {
            self.resolve_expr(arg)?;
        }

        Ok(())
    }

    // Internal helpers

    fn declare(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(&name, false);
        }
    }

    fn define(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(&name, true);
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

pub fn resolve(
    globals: &[String],
    block: &mut parser::Block,
) -> Result<(), Error> {
    let mut resolver: Resolver = Default::default();

    // Scope for static globals only starts, never ends.
    resolver.begin_scope();
    for global in globals {
        resolver.define(global);
    }

    resolver.resolve(block)
}
