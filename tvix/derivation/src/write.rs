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

fn append_array_elements(
    buffer: &mut String,
    quote: bool,
    open: &str,
    closing: &str,
    elements: &[String],
) {
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
}

pub fn append_outputs(buffer: &mut String, outputs: &BTreeMap<String, Output>) {
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

        append_array_elements(buffer, true, PAREN_OPEN, PAREN_CLOSE, &elements);
    }
    buffer.push_str(BRACKET_CLOSE);
}

pub fn append_input_derivations(
    buffer: &mut String,
    input_derivations: &BTreeMap<String, Vec<String>>,
) {
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

        append_array_elements(buffer, true, BRACKET_OPEN, BRACKET_CLOSE, input_derivation);

        buffer.push_str(PAREN_CLOSE);
    }

    buffer.push_str(BRACKET_CLOSE);
}

pub fn append_input_sources(buffer: &mut String, input_sources: &[String]) {
    buffer.push_str(COMMA);
    append_array_elements(buffer, true, BRACKET_OPEN, BRACKET_CLOSE, input_sources)
}

pub fn append_platfrom(buffer: &mut String, platform: &str) {
    buffer.push_str(COMMA);
    buffer.push_str(&escape_string(platform));
}

pub fn append_builder(buffer: &mut String, builder: &str) {
    buffer.push_str(COMMA);
    buffer.push_str(&escape_string(builder));
}

pub fn append_arguments(buffer: &mut String, arguments: &[String]) {
    buffer.push_str(COMMA);
    append_array_elements(buffer, true, BRACKET_OPEN, BRACKET_CLOSE, arguments)
}

pub fn append_environment(buffer: &mut String, environment: &BTreeMap<String, String>) {
    buffer.push_str(COMMA);
    buffer.push_str(BRACKET_OPEN);

    for (ii, (key, environment)) in environment.iter().enumerate() {
        if ii > 0 {
            buffer.push_str(COMMA);
        }

        // TODO(jrhahn) add strip option
        append_array_elements(
            buffer,
            false,
            PAREN_OPEN,
            PAREN_CLOSE,
            &[
                escape_string(key).to_string(),
                escape_string(environment).to_string(),
            ],
        );
    }

    buffer.push_str(BRACKET_CLOSE);
}
