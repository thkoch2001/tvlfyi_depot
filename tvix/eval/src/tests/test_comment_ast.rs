use expect_test::expect_file;
use std::ffi::OsStr;
use std::fs;
use std::path::PathBuf;

use rnix::ast::{self};
use rnix::{match_ast, SyntaxNode, WalkEvent};
use rowan::ast::AstNode;
use std::fmt::Write;

use crate::compiler::comment;

fn dir_tests<F>(get_actual: F)
where
    F: Fn(String) -> String,
{
    let base_path: PathBuf = [env!("CARGO_MANIFEST_DIR"), "src/tests/doc_comments"]
        .iter()
        .collect();
    let ok_path = base_path.join("ok");
    let none_path = base_path.join("none");

    let entries = ok_path
        .read_dir()
        .unwrap()
        .chain(none_path.read_dir().unwrap());

    for entry in entries {
        let path = entry.unwrap().path();

        if path.extension() != Some(OsStr::new("nix")) {
            continue;
        }

        println!("testing: {}", path.display());

        let mut code = fs::read_to_string(&path).unwrap();
        if code.ends_with('\n') {
            code.truncate(code.len() - 1);
        }

        let actual = get_actual(code);

        expect_file![path.with_extension("expect")].assert_eq(&actual);
    }
}

/// This filters out certain [SyntaxNode]s
///
/// Currently doc-comments are only
/// supported yet for [ast::Lambda] and [ast::Apply]
///
fn filter_node(node: SyntaxNode) -> Option<SyntaxNode> {
    return match_ast! {
        match node {
            // Function declaration
            ast::Lambda(_) => {
                Some(node)
            },
            // Function application creates a new function from a curried function.
            ast::Apply(_) => {
                Some(node)
            },

            _ => None,
        }
    };
}

/// Tests the [comment::get_expr_docs] function.
///
/// Test steps:
///
/// 1. Iterate through every node in the ast
/// 2. Call [comment::get_expr_docs] on every node
/// 2. Check if the formated result matches
///
#[test]
fn test_doc_comment() {
    dir_tests(|code| {
        let mut actual = String::new();
        let parse = rnix::Root::parse(&code);
        let nix = parse.ok().expect("failed to parse input");

        nix.syntax()
            .preorder()
            .filter_map(|ev| match ev {
                WalkEvent::Enter(n) => Some(n),
                _ => None,
            })
            .filter_map(filter_node)
            .map(|node| match comment::get_expr_docs(&node) {
                Some(s) => (Some(s), node),
                None => (None, node),
            })
            .for_each(|(doc, node)| {
                //append the formated string
                writeln!(actual, "{:?} - {:?}", node, doc);
            });

        actual
    })
}
