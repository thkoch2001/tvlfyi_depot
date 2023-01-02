mod nix_hash;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, fmt, fmt::Write, iter::FromIterator};
use tvix_store::nixbase32::NIXBASE32;

#[cfg(test)]
mod tests;

const DERIVATION_PREFIX: &str = "Derive";
const PAREN_OPEN: char = '(';
const PAREN_CLOSE: char = ')';
const BRACKET_OPEN: char = '[';
const BRACKET_CLOSE: char = ']';
const COMMA: char = ',';
const QUOTE: char = '"';

const COLON: &str = ":";
const TEXT_COLON: &str = "text:";
const SHA256_COLON: &str = "sha256:";
const STORE_PATH_COLON: &str = "/nix/store:";
const DOT_FILE_EXT: &str = ".drv";

const STRING_ESCAPER: [(char, &str); 5] = [
    ('\\', "\\\\"),
    ('\n', "\\n"),
    ('\r', "\\r"),
    ('\t', "\\t"),
    ('\"', "\\\""),
];

/// this is necessary to teach serde what to do in case fields are not present
/// in the JSON it's trying to parse.
fn default_resource() -> String {
    "".to_string()
}

#[derive(Serialize, Deserialize)]
pub struct Output {
    path: String,
    #[serde(default = "default_resource")]
    hash_algorithm: String,
    #[serde(default = "default_resource")]
    hash: String,
}

#[derive(Serialize, Deserialize)]
pub struct Derivation {
    outputs: BTreeMap<String, Output>,
    input_sources: Vec<String>,
    input_derivations: BTreeMap<String, Vec<String>>,
    platform: String,
    builder: String,
    arguments: Vec<String>,
    environment: BTreeMap<String, String>,
}

impl Derivation {
    fn get_path(&self, name: &str) -> String {
        let mut hasher = Sha256::new();

        // step 1: input sources and derivations
        hasher.update(TEXT_COLON);

        let mut concat_inputs: Vec<String> = self.input_sources.clone();
        let input_derivation_keys: Vec<String> = self.input_derivations.keys().cloned().collect();
        concat_inputs.extend(input_derivation_keys);
        concat_inputs.sort();

        let mut concated_inputs_as_strings = concat_inputs.join(":");

        if !concat_inputs.is_empty() {
            concated_inputs_as_strings += ":";
        }
        hasher.update(concated_inputs_as_strings);

        // step 2: aterm hash
        hasher.update(SHA256_COLON);
        let mut derivation_hasher = Sha256::new();
        derivation_hasher.update(self.to_string());

        let aterm_digest = derivation_hasher.finalize();
        hasher.update(format!("{:x}", aterm_digest));
        hasher.update(COLON);

        // step 3: store path including name
        hasher.update(STORE_PATH_COLON);
        hasher.update(name);
        hasher.update(DOT_FILE_EXT);

        let aterm_digest = Vec::from_iter(hasher.finalize());
        let compressed = nix_hash::compress_hash(aterm_digest, 20);

        format!("{}-{}{}", NIXBASE32.encode(&compressed), name, DOT_FILE_EXT)
    }
}

impl ToString for Derivation {
    fn to_string(&self) -> String {
        let mut serialized_derivation = String::new();
        serialize_derivation(self, &mut serialized_derivation).expect("Serialization failed.");
        serialized_derivation
    }
}

fn escape_string(s: &str) -> String {
    let mut s_replaced = s.to_string();

    for escape_sequence in STRING_ESCAPER {
        s_replaced = s_replaced.replace(escape_sequence.0, escape_sequence.1);
    }

    format!("\"{}\"", s_replaced)
}

fn write_array_elements(
    writer: &mut impl Write,
    quote: bool,
    open: &str,
    closing: &str,
    elements: Vec<&String>,
) -> Result<(), fmt::Error> {
    writer.write_str(open)?;

    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            writer.write_char(COMMA)?;
        }

        if quote {
            writer.write_char(QUOTE)?;
        }

        writer.write_str(element)?;

        if quote {
            writer.write_char(QUOTE)?;
        }
    }

    writer.write_str(closing)?;

    Ok(())
}

pub fn serialize_derivation(
    derivation: &Derivation,
    writer: &mut impl Write,
) -> Result<(), fmt::Error> {
    writer.write_str(DERIVATION_PREFIX)?;
    writer.write_char(PAREN_OPEN)?;

    // Step 1: Write outputs
    {
        writer.write_char(BRACKET_OPEN)?;
        for (ii, (output_name, output)) in derivation.outputs.iter().enumerate() {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }

            // TODO(jrhahn) option to strip output
            let elements = vec![
                output_name,
                &output.path,
                &output.hash_algorithm,
                &output.hash,
            ];

            write_array_elements(
                writer,
                true,
                &PAREN_OPEN.to_string(),
                &PAREN_CLOSE.to_string(),
                elements,
            )?
        }
        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 2: Write input_derivations
    {
        writer.write_char(COMMA)?;
        writer.write_char(BRACKET_OPEN)?;

        for (ii, (input_derivation_path, input_derivation)) in
            derivation.input_derivations.iter().enumerate()
        {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }

            writer.write_char(PAREN_OPEN)?;
            writer.write_char(QUOTE)?;
            writer.write_str(input_derivation_path.as_str())?;
            writer.write_char(QUOTE)?;
            writer.write_char(COMMA)?;

            write_array_elements(
                writer,
                true,
                &BRACKET_OPEN.to_string(),
                &BRACKET_CLOSE.to_string(),
                input_derivation.iter().collect(),
            )?;

            writer.write_char(PAREN_CLOSE)?;
        }

        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 3: Write input_sources
    {
        writer.write_char(COMMA)?;
        write_array_elements(
            writer,
            true,
            &BRACKET_OPEN.to_string(),
            &BRACKET_CLOSE.to_string(),
            derivation.input_sources.iter().collect(),
        )?;
    }

    // Step 4: Write platform
    {
        writer.write_char(COMMA)?;
        writer.write_str(escape_string(&derivation.platform).as_str())?;
    }

    // Step 5: Write builder
    {
        writer.write_char(COMMA)?;
        writer.write_str(escape_string(&derivation.builder).as_str())?;
    }

    // Step 6: Write arguments
    {
        writer.write_char(COMMA)?;
        write_array_elements(
            writer,
            true,
            &BRACKET_OPEN.to_string(),
            &BRACKET_CLOSE.to_string(),
            derivation.arguments.iter().collect(),
        )?;
    }

    // Step 7: Write env
    {
        writer.write_char(COMMA)?;
        writer.write_char(BRACKET_OPEN)?;

        for (ii, (key, environment)) in derivation.environment.iter().enumerate() {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }

            // TODO(jrhahn) add strip option
            write_array_elements(
                writer,
                false,
                &PAREN_OPEN.to_string(),
                &PAREN_CLOSE.to_string(),
                vec![&escape_string(key), &escape_string(environment)],
            )?;
        }

        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 8: Close Derive call
    writer.write_char(PAREN_CLOSE)?;

    Ok(())
}
