extern crate semver;
extern crate toml;

use std::io::Write;

/// reads a security advisory of the form
/// https://github.com/RustSec/advisory-db/blob/a24932e220dfa9be8b0b501210fef8a0bc7ef43e/EXAMPLE_ADVISORY.md
/// and a crate version number,
/// and returns 0 if the crate version is patched
/// and returns 1 if the crate version is *not* patched
///
/// If PRINT_ADVISORY is set, the advisory is printed if it matches.

fn main() {
    let mut args = std::env::args_os();
    let file = args.nth(1).expect("security advisory md file is $1");
    let crate_version =
        args.nth(0).expect("crate version is $2")
        .into_string().expect("crate version string not utf8")
        ;
    let crate_version = semver::Version::parse(&crate_version).expect(&format!("this is not a semver version: {}", &crate_version));
    let filename = file.to_string_lossy();

    let content = std::fs::read(&file).expect(&format!("could not read {}", filename));
    let content =
        std::str::from_utf8(&content).expect(&format!("file {} was not encoded as utf-8", filename));
    let content = content.trim_start();

    let toml_start = content
        .strip_prefix("```toml").expect(&format!("file did not start with ```toml: {}", filename));
    let toml_end_index = toml_start.find("```").expect(&format!("the toml section did not end, no `` found: {}", filename));
    let toml = &toml_start[..toml_end_index];
    let toml : toml::Value = toml::de::from_slice(toml.as_bytes()).expect(&format!("could not parse toml: {}", filename));

    let versions = toml
        .as_table().expect(&format!("the toml is not a table: {}", filename))
        .get("versions").expect(&format!("the toml does not contain the versions field: {}", filename))
        .as_table().expect(&format!("the toml versions field must be a table: {}", filename));

    let unaffected = match versions.get("unaffected") {
        Some(u) => u
            .as_array().expect(&format!("the toml versions.unaffected field must be a list of semvers: {}", filename))
            .iter()
            .map(|v| semver::VersionReq::parse(v.as_str().expect(&format!("the version field {} is not a string", v))).expect(&format!("the version field {} is not a valid semver VersionReq", v)))
            .collect(),
        None => vec![]
    };

    let mut patched : Vec<semver::VersionReq> = versions.get("patched").expect(&format!("the toml versions.patched field must exist: {}", filename))
        .as_array().expect(&format!("the toml versions.patched field must be a list of semvers: {}", filename))
        .iter()
        .map(|v| semver::VersionReq::parse(v.as_str().expect(&format!("the version field {} is not a string", v))).expect(&format!("the version field {} is not a valid semver VersionReq", v)))
        .collect();

    patched.extend_from_slice(&unaffected[..]);
    let is_patched_or_unaffected = patched.iter().any(|req| req.matches(&crate_version));

    if is_patched_or_unaffected {
        std::process::exit(0);
    } else {
        if std::env::var_os("PRINT_ADVISORY").is_some() {
            write!(std::io::stderr(), "Advisory {} matched!\n{}\n", filename, content).unwrap();
        }
        std::process::exit(1);
    }

}
