#[cfg(feature = "compile-tests")]
use std::path::PathBuf;

#[cfg(feature = "compile-tests")]
use rstest::rstest;

#[cfg(feature = "compile-tests")]
#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
