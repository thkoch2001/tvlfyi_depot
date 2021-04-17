use std::convert::{TryFrom, TryInto};
use std::path::Path;
use std::result;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
pub use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use itertools::Itertools;
use thiserror::Error;

use crate::ast::hir::{Binding, Decl, Expr, Pattern};
use crate::ast::{BinaryOperator, Ident, Literal, Type, UnaryOperator};
use crate::common::env::Env;

#[derive(Debug, PartialEq, Eq, Error)]
pub enum Error {
    #[error("Undefined variable {0}")]
    UndefinedVariable(Ident<'static>),

    #[error("LLVM Error: {0}")]
    LLVMError(String),
}

impl From<LLVMString> for Error {
    fn from(s: LLVMString) -> Self {
        Self::LLVMError(s.to_string())
    }
}

pub type Result<T> = result::Result<T, Error>;

pub struct Codegen<'ctx, 'ast> {
    context: &'ctx Context,
    pub module: Module<'ctx>,
    builder: Builder<'ctx>,
    env: Env<&'ast Ident<'ast>, AnyValueEnum<'ctx>>,
    function_stack: Vec<FunctionValue<'ctx>>,
    identifier_counter: u32,
}

impl<'ctx, 'ast> Codegen<'ctx, 'ast> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            env: Default::default(),
            function_stack: Default::default(),
            identifier_counter: 0,
        }
    }

    pub fn new_function<'a>(
        &'a mut self,
        name: &str,
        ty: FunctionType<'ctx>,
    ) -> &'a FunctionValue<'ctx> {
        self.function_stack
            .push(self.module.add_function(name, ty, None));
        let basic_block = self.append_basic_block("entry");
        self.builder.position_at_end(basic_block);
        self.function_stack.last().unwrap()
    }

    pub fn finish_function(&mut self, res: Option<&BasicValueEnum<'ctx>>) -> FunctionValue<'ctx> {
        self.builder.build_return(match res {
            // lol
            Some(val) => Some(val),
            None => None,
        });
        self.function_stack.pop().unwrap()
    }

    pub fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(*self.function_stack.last().unwrap(), name)
    }

    fn bind_pattern(&mut self, pat: &'ast Pattern<'ast, Type>, val: AnyValueEnum<'ctx>) {
        match pat {
            Pattern::Id(id, _) => self.env.set(id, val),
            Pattern::Tuple(pats) => {
                for (i, pat) in pats.iter().enumerate() {
                    let member = self
                        .builder
                        .build_extract_value(
                            StructValue::try_from(val).unwrap(),
                            i as _,
                            "pat_bind",
                        )
                        .unwrap();
                    self.bind_pattern(pat, member.into());
                }
            }
        }
    }

    pub fn codegen_expr(
        &mut self,
        expr: &'ast Expr<'ast, Type>,
    ) -> Result<Option<AnyValueEnum<'ctx>>> {
        match expr {
            Expr::Ident(id, _) => self
                .env
                .resolve(id)
                .cloned()
                .ok_or_else(|| Error::UndefinedVariable(id.to_owned()))
                .map(Some),
            Expr::Literal(lit, ty) => {
                let ty = self.codegen_int_type(ty);
                match lit {
                    Literal::Int(i) => Ok(Some(AnyValueEnum::IntValue(ty.const_int(*i, false)))),
                    Literal::Bool(b) => Ok(Some(AnyValueEnum::IntValue(
                        ty.const_int(if *b { 1 } else { 0 }, false),
                    ))),
                    Literal::String(s) => Ok(Some(
                        self.builder
                            .build_global_string_ptr(s, "s")
                            .as_pointer_value()
                            .into(),
                    )),
                    Literal::Unit => Ok(None),
                }
            }
            Expr::UnaryOp { op, rhs, .. } => {
                let rhs = self.codegen_expr(rhs)?.unwrap();
                match op {
                    UnaryOperator::Not => unimplemented!(),
                    UnaryOperator::Neg => Ok(Some(AnyValueEnum::IntValue(
                        self.builder.build_int_neg(rhs.into_int_value(), "neg"),
                    ))),
                }
            }
            Expr::BinaryOp { lhs, op, rhs, .. } => {
                let lhs = self.codegen_expr(lhs)?.unwrap();
                let rhs = self.codegen_expr(rhs)?.unwrap();
                match op {
                    BinaryOperator::Add => {
                        Ok(Some(AnyValueEnum::IntValue(self.builder.build_int_add(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "add",
                        ))))
                    }
                    BinaryOperator::Sub => {
                        Ok(Some(AnyValueEnum::IntValue(self.builder.build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "add",
                        ))))
                    }
                    BinaryOperator::Mul => {
                        Ok(Some(AnyValueEnum::IntValue(self.builder.build_int_sub(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "add",
                        ))))
                    }
                    BinaryOperator::Div => Ok(Some(AnyValueEnum::IntValue(
                        self.builder.build_int_signed_div(
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "add",
                        ),
                    ))),
                    BinaryOperator::Pow => unimplemented!(),
                    BinaryOperator::Equ => Ok(Some(AnyValueEnum::IntValue(
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "eq",
                        ),
                    ))),
                    BinaryOperator::Neq => todo!(),
                }
            }
            Expr::Let { bindings, body, .. } => {
                self.env.push();
                for Binding { pat, body, .. } in bindings {
                    if let Some(val) = self.codegen_expr(body)? {
                        self.bind_pattern(pat, val);
                    }
                }
                let res = self.codegen_expr(body);
                self.env.pop();
                res
            }
            Expr::If {
                condition,
                then,
                else_,
                type_,
            } => {
                let then_block = self.append_basic_block("then");
                let else_block = self.append_basic_block("else");
                let join_block = self.append_basic_block("join");
                let condition = self.codegen_expr(condition)?.unwrap();
                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_block,
                    else_block,
                );
                self.builder.position_at_end(then_block);
                let then_res = self.codegen_expr(then)?;
                self.builder.build_unconditional_branch(join_block);

                self.builder.position_at_end(else_block);
                let else_res = self.codegen_expr(else_)?;
                self.builder.build_unconditional_branch(join_block);

                self.builder.position_at_end(join_block);
                if let Some(phi_type) = self.codegen_type(type_) {
                    let phi = self.builder.build_phi(phi_type, "join");
                    phi.add_incoming(&[
                        (
                            &BasicValueEnum::try_from(then_res.unwrap()).unwrap(),
                            then_block,
                        ),
                        (
                            &BasicValueEnum::try_from(else_res.unwrap()).unwrap(),
                            else_block,
                        ),
                    ]);
                    Ok(Some(phi.as_basic_value().into()))
                } else {
                    Ok(None)
                }
            }
            Expr::Call { fun, args, .. } => {
                if let Expr::Ident(id, _) = &**fun {
                    let function = self
                        .module
                        .get_function(id.into())
                        .or_else(|| self.env.resolve(id)?.clone().try_into().ok())
                        .ok_or_else(|| Error::UndefinedVariable(id.to_owned()))?;
                    let args = args
                        .iter()
                        .map(|arg| Ok(self.codegen_expr(arg)?.unwrap().try_into().unwrap()))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(self
                        .builder
                        .build_call(function, &args, "call")
                        .try_as_basic_value()
                        .left()
                        .map(|val| val.into()))
                } else {
                    todo!()
                }
            }
            Expr::Fun { args, body, .. } => {
                let fname = self.fresh_ident("f");
                let cur_block = self.builder.get_insert_block().unwrap();
                let env = self.env.save(); // TODO: closures
                let function = self.codegen_function(&fname, args, body)?;
                self.builder.position_at_end(cur_block);
                self.env.restore(env);
                Ok(Some(function.into()))
            }
            Expr::Tuple(members, ty) => {
                let values = members
                    .into_iter()
                    .map(|expr| self.codegen_expr(expr))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .filter_map(|x| x)
                    .map(|x| x.try_into().unwrap())
                    .collect_vec();
                let field_types = ty.as_tuple().unwrap();
                let tuple_type = self.codegen_tuple_type(field_types);
                Ok(Some(tuple_type.const_named_struct(&values).into()))
            }
        }
    }

    pub fn codegen_function(
        &mut self,
        name: &str,
        args: &'ast [(Ident<'ast>, Type)],
        body: &'ast Expr<'ast, Type>,
    ) -> Result<FunctionValue<'ctx>> {
        let arg_types = args
            .iter()
            .filter_map(|(_, at)| self.codegen_type(at))
            .collect::<Vec<_>>();

        self.new_function(
            name,
            match self.codegen_type(body.type_()) {
                Some(ret_ty) => ret_ty.fn_type(&arg_types, false),
                None => self.context.void_type().fn_type(&arg_types, false),
            },
        );
        self.env.push();
        for (i, (arg, _)) in args.iter().enumerate() {
            self.env.set(
                arg,
                self.cur_function().get_nth_param(i as u32).unwrap().into(),
            );
        }
        let res = self.codegen_expr(body)?;
        self.env.pop();
        Ok(self.finish_function(res.map(|av| av.try_into().unwrap()).as_ref()))
    }

    pub fn codegen_extern(
        &mut self,
        name: &str,
        args: &'ast [Type],
        ret: &'ast Type,
    ) -> Result<()> {
        let arg_types = args
            .iter()
            .map(|t| self.codegen_type(t).unwrap())
            .collect::<Vec<_>>();
        self.module.add_function(
            name,
            match self.codegen_type(ret) {
                Some(ret_ty) => ret_ty.fn_type(&arg_types, false),
                None => self.context.void_type().fn_type(&arg_types, false),
            },
            None,
        );
        Ok(())
    }

    pub fn codegen_decl(&mut self, decl: &'ast Decl<'ast, Type>) -> Result<()> {
        match decl {
            Decl::Fun {
                name, args, body, ..
            } => {
                self.codegen_function(name.into(), args, body)?;
                Ok(())
            }
            Decl::Extern {
                name,
                arg_types,
                ret_type,
            } => self.codegen_extern(name.into(), arg_types, ret_type),
        }
    }

    pub fn codegen_main(&mut self, expr: &'ast Expr<'ast, Type>) -> Result<()> {
        self.new_function("main", self.context.i64_type().fn_type(&[], false));
        let res = self.codegen_expr(expr)?;
        if *expr.type_() != Type::Int {
            self.builder
                .build_return(Some(&self.context.i64_type().const_int(0, false)));
        } else {
            self.finish_function(res.map(|r| r.try_into().unwrap()).as_ref());
        }
        Ok(())
    }

    fn codegen_type(&self, type_: &'ast Type) -> Option<BasicTypeEnum<'ctx>> {
        // TODO
        match type_ {
            Type::Int => Some(self.context.i64_type().into()),
            Type::Float => Some(self.context.f64_type().into()),
            Type::Bool => Some(self.context.bool_type().into()),
            Type::CString => Some(
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .into(),
            ),
            Type::Function(_) => todo!(),
            Type::Var(_) => unreachable!(),
            Type::Unit => None,
            Type::Tuple(ts) => Some(self.codegen_tuple_type(ts).into()),
        }
    }

    fn codegen_tuple_type(&self, ts: &'ast [Type]) -> StructType<'ctx> {
        self.context.struct_type(
            ts.iter()
                .filter_map(|t| self.codegen_type(t))
                .collect_vec()
                .as_slice(),
            false,
        )
    }

    fn codegen_int_type(&self, type_: &'ast Type) -> IntType<'ctx> {
        // TODO
        self.context.i64_type()
    }

    pub fn print_to_file<P>(&self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        Ok(self.module.print_to_file(path)?)
    }

    pub fn binary_to_file<P>(&self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        if self.module.write_bitcode_to_path(path.as_ref()) {
            Ok(())
        } else {
            Err(Error::LLVMError(
                "Error writing bitcode to output path".to_owned(),
            ))
        }
    }

    fn fresh_ident(&mut self, prefix: &str) -> String {
        self.identifier_counter += 1;
        format!("{}{}", prefix, self.identifier_counter)
    }

    fn cur_function(&self) -> &FunctionValue<'ctx> {
        self.function_stack.last().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use inkwell::execution_engine::JitFunction;
    use inkwell::OptimizationLevel;

    use super::*;

    fn jit_eval<T>(expr: &str) -> anyhow::Result<T> {
        let expr = crate::parser::expr(expr).unwrap().1;

        let expr = crate::tc::typecheck_expr(expr).unwrap();

        let context = Context::create();
        let mut codegen = Codegen::new(&context, "test");
        let execution_engine = codegen
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        codegen.codegen_function("test", &[], &expr)?;

        unsafe {
            let fun: JitFunction<unsafe extern "C" fn() -> T> =
                execution_engine.get_function("test")?;
            Ok(fun.call())
        }
    }

    #[test]
    fn add_literals() {
        assert_eq!(jit_eval::<i64>("1 + 2").unwrap(), 3);
    }

    #[test]
    fn variable_shadowing() {
        assert_eq!(
            jit_eval::<i64>("let x = 1 in (let x = 2 in x) + x").unwrap(),
            3
        );
    }

    #[test]
    fn eq() {
        assert_eq!(
            jit_eval::<i64>("let x = 1 in if x == 1 then 2 else 4").unwrap(),
            2
        );
    }

    #[test]
    fn function_call() {
        let res = jit_eval::<i64>("let id = fn x = x in id 1").unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn bind_tuple_pattern() {
        let res = jit_eval::<i64>("let (x, y) = (1, 2) in x + y").unwrap();
        assert_eq!(res, 3);
    }
}
