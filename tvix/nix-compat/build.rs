fn main() {
    // https://github.com/la10736/rstest/issues/256
    println!("cargo:rerun-if-changed=src/derivation/tests/derivation_tests")
}
