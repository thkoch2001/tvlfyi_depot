use super::*;

fn code(code: &str) -> Vec<char> {
    code.chars().collect()
}

/// Evaluate a code snippet, returning a value.
fn parse_eval<'a>(chars: &'a [char]) -> Value<'a> {
    let tokens = scanner::scan(&chars).expect("could not scan code");
    let program = parser::parse(tokens).expect("could not parse code");
    Interpreter::create()
        .interpret(&program)
        .expect("could not eval code")
}

#[test]
fn test_if() {
    let code = code(
        r#"
if (42 > 23)
  "pass";
else
  "fail";
"#,
    );

    assert_eq!(
        Value::Literal(Literal::String("pass".into())),
        parse_eval(&code)
    );
}

#[test]
fn test_scope() {
    let code = code(
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
        parse_eval(&code),
    );
}

#[test]
fn test_binary_operators() {
    assert_eq!(
        Value::Literal(Literal::Number(42.0)),
        parse_eval(&code("40 + 2;"))
    );

    assert_eq!(
        Value::Literal(Literal::String("foobar".into())),
        parse_eval(&code("\"foo\" + \"bar\";"))
    );
}
