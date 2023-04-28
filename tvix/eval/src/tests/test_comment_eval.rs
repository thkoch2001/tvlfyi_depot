use expect_test::expect_file;
use std::fs;
use std::path::PathBuf;
use test_generator::test_resources;

use crate::Evaluation;

/// Evaluate the given nix code
/// Then call [value::explain] to print the doc_comment
///
/// Returns [Some(String)] if the evaluation yields a valid value
fn eval_explain(code: &str, path: Option<PathBuf>) -> Option<String> {
    let eval = Evaluation::new_impure(code, path);

    if let Some(value) = eval.evaluate().value.as_ref() {
        Some(value.explain())
    } else {
        None
    }
}

#[test_resources("src/tests/doc_comments/eval/*.nix")]
fn test_doc_comment(path: &str) {
    let path = PathBuf::from(path);
    let mut code = fs::read_to_string(&path).unwrap();
    if code.ends_with('\n') {
        code.truncate(code.len() - 1);
    }
    let actual = eval_explain(&code, Some(path.clone())).unwrap();
    let curr_dir = std::env::current_dir().unwrap();
    let abs_path: PathBuf = vec![curr_dir, path].iter().collect();
    expect_file![abs_path.with_extension("expect")].assert_eq(&actual);
}
