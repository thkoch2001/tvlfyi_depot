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

fn expect_str(code: &str, value: &str) {
    expect(code, Value::String(value.to_string().into()))
}

#[test]
fn numbers() {
    expect_num("1;", 1.0);
    expect_num("13.37;", 13.37);
}

#[test]
fn negative_numbers() {
    // Note: This technically tests unary operators.
    expect_num("-1;", -1.0);
    expect_num("-13.37;", -13.37);
}

#[test]
fn terms() {
    expect_num("1 + 2;", 3.0);
    expect_num("3 - 1;", 2.0);
    expect_num("0.7 + 0.3;", 1.0);
    expect_num("1 + -3;", -2.0);
    expect_num("-1 - -1;", 0.0);
    expect_num("10 - -10 + 10;", 30.0);
}

#[test]
fn factors() {
    expect_num("1 * 2;", 2.0);
    expect_num("10 / 5;", 2.0);
    expect_num("0.7 * 4 / 1.4;", 2.0);
    expect_num("10 * -10 / 10;", -10.0);
}

#[test]
fn arithmetic() {
    expect_num("10 - 3 * 2;", 4.0);
    expect_num("-4 * -4 + (14 - 5);", 25.0);
    expect_num("(702 + 408) - ((239 - 734) / -5) + -4;", 1007.0);
}

#[test]
fn trivial_literals() {
    expect("true;", Value::Bool(true));
    expect("false;", Value::Bool(false));
    expect("nil;", Value::Nil);
}

#[test]
fn negation() {
    expect_bool("!true;", false);
    expect_bool("!false;", true);
    expect_bool("!nil;", true);
    expect_bool("!13.5;", false);
    expect_bool("!-42;", false);
}

#[test]
fn equality() {
    expect_bool("42 == 42;", true);
    expect_bool("42 != 42;", false);
    expect_bool("42 == 42.0;", true);

    expect_bool("true == true;", true);
    expect_bool("true == false;", false);
    expect_bool("true == !false;", true);
    expect_bool("true != true;", false);
    expect_bool("true != false;", true);

    expect_bool("42 == false;", false);
    expect_bool("42 == true;", false);
    expect_bool("!42 == !true;", true);
}

#[test]
fn comparisons() {
    expect_bool("42 > 23;", true);
    expect_bool("42 < 23;", false);
    expect_bool("42 <= 42;", true);
    expect_bool("42 <= 23;", false);
    expect_bool("42 >= 42;", true);
    expect_bool("42 >= 23;", true);
}

#[test]
fn strings() {
    expect_str("\"hello\";", "hello");
    expect_str("\"hello\" + \" world\";", "hello world");
}

#[test]
fn global_variables() {
    expect_num("var a = 5; a;", 5.0);
    expect_num("var a = 5; var b = 2; a * b;", 10.0);
    expect_str(
        "var greeting = \"hello\"; var name = \"Zubnog\"; greeting + \" \" + name;",
        "hello Zubnog",
    );
}

#[test]
fn global_assignment() {
    expect_str(
        r#"
          var breakfast = "beignets";
          var beverage = "cafe au lait";
          breakfast = "beignets with " + beverage;
          breakfast;
        "#,
        "beignets with cafe au lait",
    );
}

#[test]
fn local_variables() {
    expect_num(
        r#"
          var a = 10;
          var b = 5;
          var result = 0;
          {
            var b = 10;
            var c = 2;
            result = a * b * c;
          }

          result;
        "#,
        200.0,
    );
}
