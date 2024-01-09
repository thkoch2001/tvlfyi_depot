use serde::Deserialize;
use std::collections::HashMap;
use tvix_eval::builtin_macros::builtins;

use crate::de::{from_str, from_str_impure, from_str_with_config};

#[test]
fn deserialize_none() {
    let result: Option<usize> = from_str("null").expect("should deserialize");
    assert_eq!(None, result);
}

#[test]
fn deserialize_some() {
    let result: Option<usize> = from_str("40 + 2").expect("should deserialize");
    assert_eq!(Some(42), result);
}

#[test]
fn deserialize_string() {
    let result: String = from_str(
        r#"
      let greeter = name: "Hello ${name}!";
      in greeter "Slartibartfast"
    "#,
    )
    .expect("should deserialize");

    assert_eq!(result, "Hello Slartibartfast!");
}

#[test]
fn deserialize_empty_list() {
    let result: Vec<usize> = from_str("[ ]").expect("should deserialize");
    assert!(result.is_empty())
}

#[test]
fn deserialize_integer_list() {
    let result: Vec<usize> =
        from_str("builtins.map (n: n + 2) [ 21 40 67 ]").expect("should deserialize");
    assert_eq!(result, vec![23, 42, 69]);
}

#[test]
fn deserialize_empty_map() {
    let result: HashMap<String, usize> = from_str("{ }").expect("should deserialize");
    assert!(result.is_empty());
}

#[test]
fn deserialize_integer_map() {
    let result: HashMap<String, usize> = from_str("{ age = 40 + 2; }").expect("should deserialize");
    assert_eq!(result.len(), 1);
    assert_eq!(*result.get("age").unwrap(), 42);
}

#[test]
fn deserialize_struct() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Person {
        name: String,
        age: usize,
    }

    let result: Person = from_str(
        r#"
    {
      name = "Slartibartfast";
      age = 42;
    }
    "#,
    )
    .expect("should deserialize");

    assert_eq!(
        result,
        Person {
            name: "Slartibartfast".into(),
            age: 42,
        }
    );
}

#[test]
fn deserialize_newtype() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Number(usize);

    let result: Number = from_str("42").expect("should deserialize");
    assert_eq!(result, Number(42));
}

#[test]
fn deserialize_tuple() {
    let result: (String, usize) = from_str(r#" [ "foo" 42 ] "#).expect("should deserialize");
    assert_eq!(result, ("foo".into(), 42));
}

#[test]
fn deserialize_unit_enum() {
    #[derive(Debug, Deserialize, PartialEq)]
    enum Foo {
        Bar,
        Baz,
    }

    let result: Foo = from_str("\"Baz\"").expect("should deserialize");
    assert_eq!(result, Foo::Baz);
}

#[test]
fn deserialize_tuple_enum() {
    #[derive(Debug, Deserialize, PartialEq)]
    enum Foo {
        Bar,
        Baz(String, usize),
    }

    let result: Foo = from_str(
        r#"
    {
      Baz = [ "Slartibartfast" 42 ];
    }
    "#,
    )
    .expect("should deserialize");

    assert_eq!(result, Foo::Baz("Slartibartfast".into(), 42));
}

#[test]
fn deserialize_struct_enum() {
    #[derive(Debug, Deserialize, PartialEq)]
    enum Foo {
        Bar,
        Baz { name: String, age: usize },
    }

    let result: Foo = from_str(
        r#"
    {
      Baz.name = "Slartibartfast";
      Baz.age = 42;
    }
    "#,
    )
    .expect("should deserialize");

    assert_eq!(
        result,
        Foo::Baz {
            name: "Slartibartfast".into(),
            age: 42
        }
    );
}

#[test]
fn deserialize_enum_all() {
    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(rename_all = "snake_case")]
    enum TestEnum {
        Unit,
        Tuple(String, String),
        Struct { name: String, age: usize },
    }

    let result: Vec<TestEnum> = from_str(
        r#"
      let
        mkTuple = country: drink: { tuple = [ country drink ]; };
      in
      [
        (mkTuple "UK" "cask ale")

        "unit"

        {
          struct.name = "Slartibartfast";
          struct.age = 42;
        }

        (mkTuple "Russia" "квас")
      ]
    "#,
    )
    .expect("should deserialize");

    let expected = vec![
        TestEnum::Tuple("UK".into(), "cask ale".into()),
        TestEnum::Unit,
        TestEnum::Struct {
            name: "Slartibartfast".into(),
            age: 42,
        },
        TestEnum::Tuple("Russia".into(), "квас".into()),
    ];

    assert_eq!(result, expected);
}

#[test]
fn deserialize_with_impure_eval() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Number(usize);

    let tokio_runtime = tokio::runtime::Runtime::new().expect("failed to setup tokio runtime");
    let (blob_service, directory_service, path_info_service) = tokio_runtime
        .block_on({
            let blob_service_addr = "memory://";
            let directory_service_addr = "memory://";
            let path_info_service_addr = "memory://";
            async move {
                tvix_store::utils::construct_services(
                    blob_service_addr,
                    directory_service_addr,
                    path_info_service_addr,
                )
                .await
            }
        })
        .expect("unable to setup {blob|directory|pathinfo}service before interpreter setup");

    let io_handler = Box::new(tvix_glue::tvix_io::TvixIO::new(
        tvix_glue::tvix_store_io::TvixStoreIO::new(
            blob_service,
            directory_service,
            path_info_service,
            tokio_runtime.handle().clone(),
        ),
    ));

    let result: String =
        from_str_impure("(import ./src/test.nix).cat", io_handler).expect("should deserialize");

    assert_eq!(result, "meow!");
}

#[test]
fn deserialize_with_config() {
    let result: String = from_str_with_config("builtins.testWithConfig", |eval| {
        // Add a literal string builtin that just returns `"ok"`.
        eval.src_builtins.push(("testWithConfig", "\"ok\""));
    })
    .expect("should deserialize");

    assert_eq!(result, "ok");
}

#[builtins]
mod test_builtins {
    use tvix_eval::generators::{Gen, GenCo};
    use tvix_eval::{ErrorKind, NixString, Value};

    #[builtin("prependHello")]
    pub async fn builtin_prepend_hello(co: GenCo, x: Value) -> Result<Value, ErrorKind> {
        match x {
            Value::String(s) => {
                let new_string = NixString::from(format!("hello {}", s.as_str()));
                Ok(Value::String(new_string))
            }
            _ => Err(ErrorKind::TypeError {
                expected: "string",
                actual: "not string",
            }),
        }
    }
}

#[test]
fn deserialize_with_extra_builtin() {
    let code = "builtins.prependHello \"world\"";

    let result: String = from_str_with_config(code, |eval| {
        eval.builtins.append(&mut test_builtins::builtins());
    })
    .expect("should deserialize");

    assert_eq!(result, "hello world");
}
