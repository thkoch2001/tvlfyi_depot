// use itertools::Itertools;
use std::{collections::BTreeMap, fmt, fmt::Write};

const DERIVATION_PREFIX: &str = "Derive";
const PAREN_OPEN: char = '(';
const PAREN_CLOSE: char = ')';
const BRACKET_OPEN: char = '[';
const BRACKET_CLOSE: char = ']';
const COMMA: char = ',';
const QUOTE: char = '"';

const STRING_ESCAPER: [(char, &str); 5] = [
    ('\\', "\\\\"),
    ('\n', "\\n"),
    ('\r', "\\r"),
    ('\t', "\\t"),
    ('\"', "\\\""),
];

pub struct Output {
    path: String,
    hash_algorithm: String,
    hash: String,
}

pub struct Derivation {
    outputs: BTreeMap<String, Output>,
    input_sources: Vec<String>,
    input_derivations: BTreeMap<String, String>,
    platform: String,
    builder: String,
    arguments: Vec<String>,
    environment: BTreeMap<String, String>,
}

fn escape_string(s: &String) -> String {
    let mut s_replaced = s.clone();

    for escape_sequence in STRING_ESCAPER {
        s_replaced = s_replaced.replace(escape_sequence.0, escape_sequence.1);
    }

    return format!("\"{}\"", s);
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

    return Ok(());
}

pub fn serialize_derivation(
    derivation: Derivation,
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
                vec![&input_derivation],
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
            derivation.input_sources.iter().map(|s| s).collect(),
        )?;
    }

    // Step 4: Write platform
    {
        writer.write_char(COMMA)?;
        writer.write_str(&escape_string(&derivation.platform).as_str())?;
    }

    // Step 5: Write builder
    {
        writer.write_char(COMMA)?;
        writer.write_str(&escape_string(&derivation.builder).as_str())?;
    }

    // Step 6: Write arguments
    {
        writer.write_char(COMMA)?;
        write_array_elements(
            writer,
            true,
            &BRACKET_OPEN.to_string(),
            &BRACKET_CLOSE.to_string(),
            derivation.arguments.iter().map(|s| s).collect(),
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
                vec![&escape_string(key), &escape_string(&environment)],
            )?;
        }

        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 8: Close Derive call
    writer.write_char(PAREN_CLOSE)?;

    return Ok(());
}

#[cfg(test)]
mod tests {
    use super::{serialize_derivation, Derivation, Output};
    use maplit::btreemap;
    use std::collections::BTreeMap;

    #[test]
    fn write_simple_derivation() {
        let outputs = btreemap! {
            "out".to_string() =>      Output {
                path: "/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar".to_string(),
                hash_algorithm: "r:sha256".to_string(),
                hash: "08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba"
                    .to_string(),
            },
        };

        let env = btreemap! {
            "builder".to_string() => ":".to_string(),
            "name".to_string() => "bar".to_string(),
            "out".to_string() => "/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar".to_string(),
            "outputHash".to_string() => "08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba".to_string(),
            "outputHashAlgo".to_string() => "sha256".to_string(),
            "outputHashMode".to_string() =>  "recursive".to_string(),
            "system".to_string() => ":".to_string(),
        };

        let derivation = Derivation {
            outputs: outputs,
            input_sources: Vec::new(),
            input_derivations: BTreeMap::new(),
            platform: ":".to_string(),
            builder: ":".to_string(),
            arguments: Vec::new(),
            environment: env,
        };

        let expected_result = "Derive(\
        [(\
        \"out\",\"/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar\",\
        \"r:sha256\",\"08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba\"\
        )],\
        [],\
        [],\
        \":\",\
        \":\",\
        [],\
        [\
        (\"builder\",\":\"),\
        (\"name\",\"bar\"),\
        (\"out\",\"/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar\"),\
        (\"outputHash\",\"08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba\"),\
        (\"outputHashAlgo\",\"sha256\"),(\"outputHashMode\",\"recursive\"),(\"system\",\":\")])\
        "
        .to_string();

        let mut serialized = String::new();

        serialize_derivation(derivation, &mut serialized).expect(&expected_result);
    }
}
