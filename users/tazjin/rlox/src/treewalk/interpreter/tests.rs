use super::*;

/// Evaluate a code snippet, returning a value.
fn parse_eval(code: &str) -> Value {
    Interpreter::create()
        .interpret(code.into())
        .expect("could not interpret code")
}

#[test]
fn test_if() {
    let result = parse_eval(
        r#"
if (42 > 23)
  "pass";
else
  "fail";
"#,
    );

    assert_eq!(Value::Literal(Literal::String("pass".into())), result,);
}

#[test]
fn test_scope() {
    let result = parse_eval(
        r#"
var result = "";

var a = "global a, ";
var b = "global b, ";
var c = "global c";

{
  var a = "outer a, ";
  var b = "outer b, ";

  {
    var a = "inner a, ";
    result = a + b + c;
  }
}
"#,
    );

    assert_eq!(
        Value::Literal(Literal::String("inner a, outer b, global c".into())),
        result,
    );
}

#[test]
fn test_binary_operators() {
    assert_eq!(Value::Literal(Literal::Number(42.0)), parse_eval("40 + 2;"));

    assert_eq!(
        Value::Literal(Literal::String("foobar".into())),
        parse_eval("\"foo\" + \"bar\";")
    );
}

#[test]
fn test_functions() {
    let result = parse_eval(
        r#"
fun add(a, b, c) {
  a + b + c;
}

add(1, 2, 3);
"#,
    );

    assert_eq!(Value::Literal(Literal::Number(6.0)), result);
}

#[test]
fn test_closure() {
    let result = parse_eval(
        r#"
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
  }

  return count;
}

var counter = makeCounter();
counter(); // "1".
counter(); // "2".
"#,
    );

    assert_eq!(Value::Literal(Literal::Number(2.0)), result);
}
