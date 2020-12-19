use crate::errors::{report, Error};
use crate::parser::{self, Expr, Literal};
use crate::scanner::{self, Token, TokenKind};

// Run some Lox code and print it to stdout
pub fn run(code: &str) {
    let chars: Vec<char> = code.chars().collect();

    match scanner::scan(&chars) {
        Ok(tokens) => {
            print_tokens(&tokens);
            match parser::parse(tokens) {
                Ok(expr) => {
                    println!("Expression:\n{:?}", expr);
                    println!("Result: {:?}", eval(&expr));
                }
                Err(errors) => report_errors(errors),
            }
        }
        Err(errors) => report_errors(errors),
    }
}

fn print_tokens<'a>(tokens: &Vec<Token<'a>>) {
    println!("Tokens:");
    for token in tokens {
        println!("{:?}", token);
    }
}

fn report_errors(errors: Vec<Error>) {
    for error in errors {
        report(&error);
    }
}

// Tree-walk interpreter

fn eval_truthy(lit: &Literal) -> bool {
    match lit {
        Literal::Nil => false,
        Literal::Boolean(b) => *b,
        _ => true,
    }
}

fn eval_unary<'a>(expr: &parser::Unary<'a>) -> Literal {
    let right = eval(&*expr.right);

    match (&expr.operator.kind, right) {
        (TokenKind::Minus, Literal::Number(num)) => Literal::Number(-num),
        (TokenKind::Bang, right) => Literal::Boolean(!eval_truthy(&right)),
        _ => unimplemented!("no handling of type errors"),
    }
}

fn eval_binary<'a>(expr: &parser::Binary<'a>) -> Literal {
    let left = eval(&*expr.left);
    let right = eval(&*expr.right);

    match (&expr.operator.kind, left, right) {
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
        (TokenKind::LessEqual, Literal::Number(l), Literal::Number(r)) => Literal::Boolean(l <= r),

        // Equality
        (TokenKind::Equal, l, r) => Literal::Boolean(l == r),
        (TokenKind::BangEqual, l, r) => Literal::Boolean(l != r),

        _ => unimplemented!("type errors unhandled"),
    }
}

fn eval<'a>(expr: &Expr<'a>) -> Literal {
    match expr {
        Expr::Literal(lit) => lit.clone(),
        Expr::Grouping(grouping) => eval(&*grouping.0),
        Expr::Unary(unary) => eval_unary(unary),
        Expr::Binary(binary) => eval_binary(binary),
    }
}
