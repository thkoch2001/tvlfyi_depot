use expect_test::expect_file;
use std::fs;
use std::path::PathBuf;
use test_generator::test_resources;

use rnix::ast::{self};
use rnix::{match_ast, SyntaxNode, WalkEvent};
use rowan::ast::AstNode;
use std::fmt::Write;

use crate::compiler::comment;

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
/// Runs tests on all *.nix files in the 'doc_comments' folder
/// The expected results are denoted in *.expect files respectively
///
/// To update the expect files run:
/// `env UPDATE_EXPECT=1 cargo test`
///
/// Test steps:
///
/// 1. Iterate through every node in the ast
/// 2. Call [comment::get_expr_docs] on every node
/// 2. Check if the formated result matches
///
#[test_resources("src/tests/doc_comments/ast/*/*.nix")]
fn test_doc_comment(path: &str) {
    let path = PathBuf::from(path);
    let mut code = fs::read_to_string(&path).unwrap();
    if code.ends_with('\n') {
        code.truncate(code.len() - 1);
    }
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

    let curr_dir = std::env::current_dir().unwrap();
    let abs_path: PathBuf = vec![curr_dir, path].iter().collect();
    // expect file needs either a
    // relative path to the expect file
    // or an absolute path (<- We chose this method)
    expect_file![abs_path.with_extension("expect")].assert_eq(&actual);
}
