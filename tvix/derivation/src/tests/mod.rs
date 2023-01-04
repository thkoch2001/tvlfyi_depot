use crate::derivation::Derivation;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use test_case::test_case;
use test_generator::test_resources;
use tvix_store::nixpath::NixPath;

const RESOURCES_PATHS: &str = "src/tests/derivation_tests";

fn read_file(path: &str) -> String {
    let path = Path::new(path);
    let mut file = File::open(path).unwrap();
    let mut data = String::new();

    file.read_to_string(&mut data).unwrap();

    return data;
}

#[test_resources("src/tests/derivation_tests/*.drv")]
fn check_serizaliation(path_to_drv_file: &str) {
    let data = read_file(&format!("{}.json", path_to_drv_file));
    let derivation: Derivation = serde_json::from_str(&data).expect("JSON was not well-formatted");

    let mut serialized_derivation = String::new();
    derivation.serialize(&mut serialized_derivation).unwrap();

    let expected = read_file(path_to_drv_file);

    assert_eq!(expected, serialized_derivation);
}

#[test_resources("src/tests/derivation_tests/*.drv")]
fn validate(path_to_drv_file: &str) {
    let data = read_file(&format!("{}.json", path_to_drv_file));
    let derivation: Derivation = serde_json::from_str(&data).expect("JSON was not well-formatted");

    derivation
        .validate()
        .expect("derivation failed to validate")
}

#[test_resources("src/tests/derivation_tests/*.drv")]
fn check_to_string(path_to_drv_file: &str) {
    let data = read_file(&format!("{}.json", path_to_drv_file));
    let derivation: Derivation = serde_json::from_str(&data).expect("JSON was not well-formatted");

    let expected = read_file(path_to_drv_file);

    assert_eq!(expected, derivation.to_string());
}

#[test_case("bar","0hm2f1psjpcwg8fijsmr4wwxrx59s092-bar.drv"; "fixed_sha256")]
#[test_case("foo", "4wvvbi4jwn0prsdxb7vs673qa5h9gr7x-foo.drv"; "simple-sha256")]
#[test_case("bar", "ss2p4wmxijn652haqyd7dckxwl4c7hxx-bar.drv"; "fixed-sha1")]
#[test_case("foo", "ch49594n9avinrf8ip0aslidkc4lxkqv-foo.drv"; "simple-sha1")]
#[test_case("has-multi-out", "h32dahq0bx5rp1krcdx3a53asj21jvhk-has-multi-out.drv"; "multiple-outputs")]
#[test_case("structured-attrs", "9lj1lkjm2ag622mh4h9rpy6j607an8g2-structured-attrs.drv"; "structured-attrs")]
#[test_case("unicode", "52a9id8hx688hvlnz4d1n25ml1jdykz0-unicode.drv"; "unicode")]
fn derivation_path(name: &str, expected_path: &str) {
    let data = read_file(&format!("{}/{}.json", RESOURCES_PATHS, expected_path));
    let derivation: Derivation = serde_json::from_str(&data).expect("JSON was not well-formatted");

    assert_eq!(
        derivation.calculate_derivation_path(name),
        NixPath::from_string(expected_path).unwrap()
    );
}

#[test_case("bar","0hm2f1psjpcwg8fijsmr4wwxrx59s092-bar.drv"; "fixed_sha256")]
#[test_case("foo", "4wvvbi4jwn0prsdxb7vs673qa5h9gr7x-foo.drv"; "simple-sha256")]
#[test_case("bar", "ss2p4wmxijn652haqyd7dckxwl4c7hxx-bar.drv"; "fixed-sha1")]
#[test_case("foo", "ch49594n9avinrf8ip0aslidkc4lxkqv-foo.drv"; "simple-sha1")]
#[test_case("has-multi-out", "h32dahq0bx5rp1krcdx3a53asj21jvhk-has-multi-out.drv"; "multiple-outputs")]
#[test_case("structured-attrs", "9lj1lkjm2ag622mh4h9rpy6j607an8g2-structured-attrs.drv"; "structured-attrs")]
#[test_case("unicode", "52a9id8hx688hvlnz4d1n25ml1jdykz0-unicode.drv"; "unicode")]
fn output_paths(name: &str, expected_path: &str) {
    // read in the fixture
    let data = read_file(&format!("{}/{}.json", RESOURCES_PATHS, expected_path));
    let mut derivation: Derivation =
        serde_json::from_str(&data).expect("JSON was not well-formatted");

    let expected_derivation = derivation.clone();

    // set outputs[$outputName].path to an empty string for all outputs,
    // and delete environment[$outputName].
    let mut trimmed_env = derivation.environment.clone();
    let mut trimmed_outputs = derivation.outputs.clone();

    for (output_name, _) in derivation.outputs {
        trimmed_env.remove(&output_name);

        let trimmed_output = trimmed_outputs.get_mut(&output_name).unwrap();
        trimmed_output.path = "".to_string();
    }

    // replace environment and outputs with the trimmed variants
    derivation.environment = trimmed_env;
    derivation.outputs = trimmed_outputs;

    // invoke calculate_output_paths
    derivation.calculate_output_paths(&name).unwrap();

    // The derivation should now look like it was before
    assert_eq!(expected_derivation, derivation);
}
