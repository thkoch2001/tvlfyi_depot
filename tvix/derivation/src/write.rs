use crate::output::Output;
use crate::string_escape::escape_string;
use std::collections::BTreeMap;

pub const DERIVATION_PREFIX: &str = "Derive";
pub const PAREN_OPEN: &str = "(";
pub const PAREN_CLOSE: &str = ")";
pub const BRACKET_OPEN: &str = "[";
pub const BRACKET_CLOSE: &str = "]";
pub const COMMA: &str = ",";
pub const QUOTE: &str = "\"";

pub const COLON: &str = ":";
pub const TEXT_COLON: &str = "text:";
pub const SHA256_COLON: &str = "sha256:";
pub const STORE_PATH_COLON: &str = "/nix/store:";
pub const DOT_FILE_EXT: &str = ".drv";

fn write_array_elements(quote: bool, open: &str, closing: &str, elements: &[String]) -> String {
    let mut buffer = String::new();
    buffer.push_str(open);

    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            buffer.push_str(COMMA);
        }

        if quote {
            buffer.push_str(QUOTE);
        }

        buffer.push_str(element);

        if quote {
            buffer.push_str(QUOTE);
        }
    }

    buffer.push_str(closing);

    buffer
}

pub fn write_outputs(outputs: &BTreeMap<String, Output>) -> String {
    let mut buffer = String::new();

    buffer.push_str(BRACKET_OPEN);
    for (ii, (output_name, output)) in outputs.iter().enumerate() {
        if ii > 0 {
            buffer.push_str(COMMA);
        }

        // TODO(jrhahn) option to strip output
        let elements: [String; 4] = [
            output_name.to_string(),
            output.path.to_string(),
            output.hash_algorithm.to_string(),
            output.hash.to_string(),
        ];

        buffer.push_str(&write_array_elements(
            true,
            PAREN_OPEN,
            PAREN_CLOSE,
            &elements,
        ));
    }
    buffer.push_str(BRACKET_CLOSE);

    buffer
}

pub fn write_input_derivations(input_derivations: &BTreeMap<String, Vec<String>>) -> String {
    let mut buffer = String::new();
    buffer.push_str(COMMA);
    buffer.push_str(BRACKET_OPEN);

    for (ii, (input_derivation_path, input_derivation)) in input_derivations.iter().enumerate() {
        if ii > 0 {
            buffer.push_str(COMMA);
        }

        buffer.push_str(PAREN_OPEN);
        buffer.push_str(QUOTE);
        buffer.push_str(input_derivation_path);
        buffer.push_str(QUOTE);
        buffer.push_str(COMMA);

        buffer.push_str(&write_array_elements(
            true,
            BRACKET_OPEN,
            BRACKET_CLOSE,
            input_derivation,
        ));

        buffer.push_str(PAREN_CLOSE);
    }

    buffer.push_str(BRACKET_CLOSE);

    buffer
}

pub fn write_input_sources(input_sources: &[String]) -> String {
    let mut buffer = String::new();

    buffer.push_str(COMMA);
    buffer.push_str(&write_array_elements(
        true,
        BRACKET_OPEN,
        BRACKET_CLOSE,
        input_sources,
    ));

    buffer
}

pub fn write_platfrom(platform: &str) -> String {
    let mut buffer = String::new();
    buffer.push_str(COMMA);
    buffer.push_str(&escape_string(platform));

    buffer
}

pub fn write_builder(builder: &str) -> String {
    let mut buffer = String::new();
    buffer.push_str(COMMA);
    buffer.push_str(&escape_string(builder));

    buffer
}

pub fn write_arguments(arguments: &[String]) -> String {
    let mut buffer = String::new();
    buffer.push_str(COMMA);
    buffer.push_str(&write_array_elements(
        true,
        BRACKET_OPEN,
        BRACKET_CLOSE,
        arguments,
    ));

    buffer
}

pub fn write_enviroment(environment: &BTreeMap<String, String>) -> String {
    let mut buffer = String::new();
    buffer.push_str(COMMA);
    buffer.push_str(BRACKET_OPEN);

    for (ii, (key, environment)) in environment.iter().enumerate() {
        if ii > 0 {
            buffer.push_str(COMMA);
        }

        // TODO(jrhahn) add strip option
        buffer.push_str(&write_array_elements(
            false,
            PAREN_OPEN,
            PAREN_CLOSE,
            &[
                escape_string(key).to_string(),
                escape_string(environment).to_string(),
            ],
        ));
    }

    buffer.push_str(BRACKET_CLOSE);

    buffer
}
