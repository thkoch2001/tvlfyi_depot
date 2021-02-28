use super::value::Value;
use super::*;

use crate::Lox;

fn expect(code: &str, value: Value) {
    let result = Interpreter::create()
        .interpret(code.into())
        .expect("evaluation failed");
    assert_eq!(result, value);
}

fn expect_num(code: &str, value: f64) {
    expect(code, Value::Number(value))
}

fn expect_bool(code: &str, value: bool) {
    expect(code, Value::Bool(value))
}

#[test]
fn numbers() {
    expect_num("1", 1.0);
    expect_num("13.37", 13.37);
}

#[test]
fn negative_numbers() {
    // Note: This technically tests unary operators.
    expect_num("-1", -1.0);
    expect_num("-13.37", -13.37);
}

#[test]
fn terms() {
    expect_num("1 + 2", 3.0);
    expect_num("3 - 1", 2.0);
    expect_num("0.7 + 0.3", 1.0);
    expect_num("1 + -3", -2.0);
    expect_num("-1 - -1", 0.0);
    expect_num("10 - -10 + 10", 30.0);
}

#[test]
fn factors() {
    expect_num("1 * 2", 2.0);
    expect_num("10 / 5", 2.0);
    expect_num("0.7 * 4 / 1.4", 2.0);
    expect_num("10 * -10 / 10", -10.0);
}

#[test]
fn arithmetic() {
    expect_num("10 - 3 * 2", 4.0);
    expect_num("-4 * -4 + (14 - 5)", 25.0);
    expect_num("(702 + 408) - ((239 - 734) / -5) + -4", 1007.0);
}

#[test]
fn trivial_literals() {
    expect("true", Value::Bool(true));
    expect("false", Value::Bool(false));
    expect("nil", Value::Nil);
}

#[test]
fn negation() {
    expect_bool("!true", false);
    expect_bool("!false", true);
    expect_bool("!nil", true);
    expect_bool("!13.5", false);
    expect_bool("!-42", false);
}

#[test]
fn equality() {
    expect_bool("42 == 42", true);
    expect_bool("42 != 42", false);
    expect_bool("42 == 42.0", true);

    expect_bool("true == true", true);
    expect_bool("true == false", false);
    expect_bool("true == !false", true);
    expect_bool("true != true", false);
    expect_bool("true != false", true);

    expect_bool("42 == false", false);
    expect_bool("42 == true", false);
    expect_bool("!42 == !true", true);
}

#[test]
fn comparisons() {
    expect_bool("42 > 23", true);
    expect_bool("42 < 23", false);
    expect_bool("42 <= 42", true);
    expect_bool("42 <= 23", false);
    expect_bool("42 >= 42", true);
    expect_bool("42 >= 23", true);
}
