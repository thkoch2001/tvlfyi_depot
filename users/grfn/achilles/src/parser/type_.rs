use nom::character::complete::{multispace0, multispace1};
use nom::{alt, delimited, do_parse, map, named, opt, separated_list0, tag, terminated, tuple};

use super::ident;
use super::util::comma;
use crate::ast::{FunctionType, Type};

named!(pub function_type(&str) -> FunctionType, do_parse!(
    tag!("fn")
        >> multispace1
        >> args: map!(opt!(terminated!(separated_list0!(
            comma,
            type_
        ), multispace1)), |args| args.unwrap_or_default())
        >> tag!("->")
        >> multispace1
        >> ret: type_
        >> (FunctionType {
            args,
            ret: Box::new(ret)
        })
));

named!(tuple_type(&str) -> Type, do_parse!(
    tag!("(")
        >> multispace0
        >> fst: type_
        >> comma
        >> rest: separated_list0!(
            comma,
            type_
        )
        >> multispace0
        >> tag!(")")
        >> ({
            let mut members = Vec::with_capacity(rest.len() + 1);
            members.push(fst);
            members.append(&mut rest.clone());
            Type::Tuple(members)
        })
));

named!(pub type_(&str) -> Type, alt!(
    tag!("int") => { |_| Type::Int } |
    tag!("float") => { |_| Type::Float } |
    tag!("bool") => { |_| Type::Bool } |
    tag!("cstring") => { |_| Type::CString } |
    tag!("()") => { |_| Type::Unit } |
    tuple_type |
    function_type => { |ft| Type::Function(ft) }|
    ident => { |id| Type::Var(id) } |
    delimited!(
        tuple!(tag!("("), multispace0),
        type_,
        tuple!(tag!(")"), multispace0)
    )
));

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::*;
    use crate::ast::Ident;

    #[test]
    fn simple_types() {
        assert_eq!(test_parse!(type_, "int"), Type::Int);
        assert_eq!(test_parse!(type_, "float"), Type::Float);
        assert_eq!(test_parse!(type_, "bool"), Type::Bool);
        assert_eq!(test_parse!(type_, "cstring"), Type::CString);
        assert_eq!(test_parse!(type_, "()"), Type::Unit);
    }

    #[test]
    fn no_arg_fn_type() {
        assert_eq!(
            test_parse!(type_, "fn -> int"),
            Type::Function(FunctionType {
                args: vec![],
                ret: Box::new(Type::Int)
            })
        );
    }

    #[test]
    fn fn_type_with_args() {
        assert_eq!(
            test_parse!(type_, "fn int, bool -> int"),
            Type::Function(FunctionType {
                args: vec![Type::Int, Type::Bool],
                ret: Box::new(Type::Int)
            })
        );
    }

    #[test]
    fn fn_taking_fn() {
        assert_eq!(
            test_parse!(type_, "fn fn int, bool -> bool, float -> float"),
            Type::Function(FunctionType {
                args: vec![
                    Type::Function(FunctionType {
                        args: vec![Type::Int, Type::Bool],
                        ret: Box::new(Type::Bool)
                    }),
                    Type::Float
                ],
                ret: Box::new(Type::Float)
            })
        )
    }

    #[test]
    fn parenthesized() {
        assert_eq!(
            test_parse!(type_, "fn (fn int, bool -> bool), float -> float"),
            Type::Function(FunctionType {
                args: vec![
                    Type::Function(FunctionType {
                        args: vec![Type::Int, Type::Bool],
                        ret: Box::new(Type::Bool)
                    }),
                    Type::Float
                ],
                ret: Box::new(Type::Float)
            })
        )
    }

    #[test]
    fn tuple() {
        assert_eq!(
            test_parse!(type_, "(int, int)"),
            Type::Tuple(vec![Type::Int, Type::Int])
        )
    }

    #[test]
    fn type_vars() {
        assert_eq!(
            test_parse!(type_, "fn x, y -> x"),
            Type::Function(FunctionType {
                args: vec![
                    Type::Var(Ident::try_from("x").unwrap()),
                    Type::Var(Ident::try_from("y").unwrap()),
                ],
                ret: Box::new(Type::Var(Ident::try_from("x").unwrap())),
            })
        )
    }
}
