use crate::output::Output;
use crate::string_escape::escape_string;
use std::{collections::BTreeMap, fmt, fmt::Write};

pub const DERIVATION_PREFIX: &str = "Derive";
pub const PAREN_OPEN: char = '(';
pub const PAREN_CLOSE: char = ')';
pub const BRACKET_OPEN: char = '[';
pub const BRACKET_CLOSE: char = ']';
pub const COMMA: char = ',';
pub const QUOTE: char = '"';

fn write_array_elements(
    writer: &mut impl Write,
    quote: bool,
    open: &str,
    closing: &str,
    elements: &[&str],
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

pub fn write_outputs(
    writer: &mut impl Write,
    outputs: &BTreeMap<String, Output>,
) -> Result<(), fmt::Error> {
    writer.write_char(BRACKET_OPEN)?;
    for (ii, (output_name, output)) in outputs.iter().enumerate() {
        if ii > 0 {
            writer.write_char(COMMA)?;
        }

        // TODO(jrhahn) option to strip output
        let elements: [&str; 4] = [
            &output_name,
            &output.path,
            &output.hash_algorithm,
            &output.hash,
        ];

        write_array_elements(
            writer,
            true,
            &PAREN_OPEN.to_string(),
            &PAREN_CLOSE.to_string(),
            &elements,
        )?
    }
    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}

pub fn write_input_derivations(
    writer: &mut impl Write,
    input_derivations: &BTreeMap<String, Vec<String>>,
) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_char(BRACKET_OPEN)?;

    for (ii, (input_derivation_path, input_derivation)) in input_derivations.iter().enumerate() {
        if ii > 0 {
            writer.write_char(COMMA)?;
        }

        writer.write_char(PAREN_OPEN)?;
        writer.write_char(QUOTE)?;
        writer.write_str(input_derivation_path.as_str())?;
        writer.write_char(QUOTE)?;
        writer.write_char(COMMA)?;

        // convert Vec<String> to [&str]
        let v: Vec<&str> = input_derivation.iter().map(|x| &**x).collect();

        write_array_elements(
            writer,
            true,
            &BRACKET_OPEN.to_string(),
            &BRACKET_CLOSE.to_string(),
            &v,
        )?;

        writer.write_char(PAREN_CLOSE)?;
    }

    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}

pub fn write_input_sources(
    writer: &mut impl Write,
    input_sources: &Vec<String>,
) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;

    // convert Vec<String> to [&str]
    let v: Vec<&str> = input_sources.iter().map(|x| &**x).collect();
    write_array_elements(
        writer,
        true,
        &BRACKET_OPEN.to_string(),
        &BRACKET_CLOSE.to_string(),
        &v,
    )?;

    Ok(())
}

pub fn write_platfrom(writer: &mut impl Write, platform: &str) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_str(escape_string(platform).as_str())?;
    Ok(())
}

pub fn write_builder(writer: &mut impl Write, builder: &str) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_str(escape_string(builder).as_str())?;
    Ok(())
}

pub fn write_arguments(writer: &mut impl Write, arguments: &Vec<String>) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    // convert Vec<String> to [&str]
    let v: Vec<&str> = arguments.iter().map(|x| &**x).collect();
    write_array_elements(
        writer,
        true,
        &BRACKET_OPEN.to_string(),
        &BRACKET_CLOSE.to_string(),
        &v,
    )?;

    Ok(())
}

pub fn write_enviroment(
    writer: &mut impl Write,
    environment: &BTreeMap<String, String>,
) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_char(BRACKET_OPEN)?;

    for (ii, (key, environment)) in environment.iter().enumerate() {
        if ii > 0 {
            writer.write_char(COMMA)?;
        }

        // TODO(jrhahn) add strip option
        write_array_elements(
            writer,
            false,
            &PAREN_OPEN.to_string(),
            &PAREN_CLOSE.to_string(),
            &[&escape_string(key), &escape_string(environment)],
        )?;
    }

    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}
