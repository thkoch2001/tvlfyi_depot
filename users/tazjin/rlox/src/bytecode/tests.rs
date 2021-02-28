use super::*;

use crate::Lox;

fn expect(code: &str, value: value::Value) {
    let result = Interpreter::create()
        .interpret(code.into())
        .expect("evaluation failed");
    assert_eq!(result, value);
}

#[test]
fn numbers() {
    expect("1", 1.0);
    expect("13.37", 13.37);
}

#[test]
fn negative_numbers() {
    // Note: This technically tests unary operators.
    expect("-1", -1.0);
    expect("-13.37", -13.37);
}

#[test]
fn terms() {
    expect("1 + 2", 3.0);
    expect("3 - 1", 2.0);
    expect("0.7 + 0.3", 1.0);
    expect("1 + -3", -2.0);
    expect("-1 - -1", 0.0);
    expect("10 - -10 + 10", 30.0);
}

#[test]
fn factors() {
    expect("1 * 2", 2.0);
    expect("10 / 5", 2.0);
    expect("0.7 * 4 / 1.4", 2.0);
    expect("10 * -10 / 10", -10.0);
}

#[test]
fn arithmetic() {
    expect("10 - 3 * 2", 4.0);
    expect("-4 * -4 + (14 - 5)", 25.0);
    expect("(702 + 408) - ((239 - 734) / -5) + -4", 1007.0);
}
