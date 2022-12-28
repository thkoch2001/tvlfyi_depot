use itertools::Itertools;
use std::{collections::HashMap, fmt::Error, fmt::Write};

const DERIVATION_PREFIX: &str = "Derive";
const PARENT_OPEN: char = '(';
const PARENT_CLOSE: char = ')';
const BRACKET_OPEN: char = '[';
const BRACKET_CLOSE: char = ']';
const COMMA: char = ',';
const QUOTE: char = '"';

struct Output {
    path: String,
    hash_algorithm: String,
    hash: String,
}

struct Derivation {
    // name: String,
    outputs: HashMap<String, Output>,
    input_sources: Vec<String>,
    input_derivations: HashMap<String, String>,
    platform: String,
    builder: String,
    arguments: Vec<String>,
    environment: HashMap<String, String>,
}

// todo unsafeBytes and stringEscaper
fn escape_string(s: &String) -> String {
    return format!("\"{}\"", s);
}

fn write_array_elements(
    writer: &mut impl Write,
    quote: bool,
    open: &str,
    closing: &str,
    elements: Vec<&String>,
) -> Result<(), Error> {
    writer.write_str(open)?;

    // todo check if writer is open/readyn
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

fn serialize_derivation(derivation: Derivation, writer: &mut impl Write) -> Result<(), Error> {
    writer.write_str(DERIVATION_PREFIX)?;
    writer.write_char(PARENT_OPEN)?;

    // Step 1: Write outputs
    {
        writer.write_char(BRACKET_OPEN)?;
        let mut ii = 0;
        for output_name in derivation.outputs.keys().sorted() {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }
            ii += 1;

            let output = &derivation.outputs[output_name];

            // todo option to strip output
            let elements = vec![
                output_name,
                &output.path,
                &output.hash_algorithm,
                &output.hash,
            ];

            write_array_elements(
                writer,
                true,
                &PARENT_OPEN.to_string(),
                &PARENT_CLOSE.to_string(),
                elements,
            )?
        }
        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 2: Write input_derivations
    {
        writer.write_char(COMMA)?;
        writer.write_char(BRACKET_OPEN)?;

        let mut ii = 0;
        for input_derivation_path in derivation.input_derivations.keys().sorted() {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }

            ii += 1;

            writer.write_char(PARENT_OPEN)?;
            writer.write_char(QUOTE)?;
            writer.write_str(input_derivation_path.as_str())?;
            writer.write_char(QUOTE)?;
            writer.write_char(COMMA)?;

            write_array_elements(
                writer,
                true,
                &BRACKET_OPEN.to_string(),
                &BRACKET_CLOSE.to_string(),
                vec![&derivation.input_derivations[input_derivation_path]],
            )?;

            writer.write_char(PARENT_CLOSE)?;
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

        let mut ii = 0;
        for key in derivation.environment.keys().sorted() {
            if ii > 0 {
                writer.write_char(COMMA)?;
            }

            ii += 1;

            // todo: treat strip option
            write_array_elements(
                writer,
                false,
                &PARENT_OPEN.to_string(),
                &PARENT_CLOSE.to_string(),
                vec![
                    &escape_string(key),
                    &escape_string(&derivation.environment[key]),
                ],
            )?;
        }

        writer.write_char(BRACKET_CLOSE)?;
    }

    // Step 8: Close Derive call
    writer.write_char(PARENT_CLOSE)?;

    return Ok(());
}

#[cfg(test)]
mod tests {
    use super::{serialize_derivation, Derivation, Output};
    use maplit::hashmap;
    use std::collections::HashMap;

    #[test]
    fn write_simple_derivation() {
        let outputs = hashmap! {
            "out".to_string() =>      Output {
                path: "/nix/store/4q0pg5zpfmznxscq3avycvf9xdvx50n3-bar".to_string(),
                hash_algorithm: "r:sha256".to_string(),
                hash: "08813cbee9903c62be4c5027726a418a300da4500b2d369d3af9286f4815ceba"
                    .to_string(),
            },
        };

        let env = hashmap! {
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
            input_derivations: HashMap::new(),
            platform: "".to_string(),
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

        match serialize_derivation(derivation, &mut serialized) {
            Ok(_) => (),
            Err(error) => panic!("Problem opening the file: {:?}", error),
        }

        assert_eq!(serialized, expected_result);
    }
}
