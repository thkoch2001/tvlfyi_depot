//! This module implements the serialisation of derivations into the
//! [ATerm][] format used by C++ Nix.
//!
//! [ATerm]: http://program-transformation.org/Tools/ATermFormat.html

use crate::derivation::output::Output;
use crate::derivation::string_escape::escape_string;
use crate::store_path::StorePath;
use crate::nixhash::NixHash;
use std::collections::BTreeSet;
use std::{collections::BTreeMap, fmt, fmt::Write};

pub const DERIVATION_PREFIX: &str = "Derive";
pub const PAREN_OPEN: char = '(';
pub const PAREN_CLOSE: char = ')';
pub const BRACKET_OPEN: char = '[';
pub const BRACKET_CLOSE: char = ']';
pub const COMMA: char = ',';
pub const QUOTE: char = '"';

fn write_array_elements<T: AsRef<str>>(
    writer: &mut impl Write,
    quote: bool,
    open: &str,
    closing: &str,
    elements: Vec<T>,
) -> Result<(), fmt::Error> {
    writer.write_str(open)?;

    for (index, element) in elements.iter().enumerate() {
        if index > 0 {
            writer.write_char(COMMA)?;
        }

        if quote {
            writer.write_char(QUOTE)?;
        }

        writer.write_str(element.as_ref())?;

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

        let mut elements: Vec<&str> = vec![output_name, &output.path];

        let (e2, e3) = match &output.hash_with_mode {
            Some(hash) => match hash {
                crate::nixhash::NixHashWithMode::Flat(h) => (
                    h.algo.to_string(),
                    data_encoding::HEXLOWER.encode(&h.digest),
                ),
                crate::nixhash::NixHashWithMode::Recursive(h) => (
                    format!("r:{}", h.algo),
                    data_encoding::HEXLOWER.encode(&h.digest),
                ),
            },
            None => ("".to_string(), "".to_string()),
        };

        elements.push(&e2);
        elements.push(&e3);

        write_array_elements(
            writer,
            true,
            &PAREN_OPEN.to_string(),
            &PAREN_CLOSE.to_string(),
            elements,
        )?
    }
    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}

/// Hook for writing the the reference to a [DerivationPoly] in `inputDrvs`.
///
/// This is needed because it is not always a store  path, can be done in a few different ways.
pub trait WriteDrvReference {
    fn write_drv_reference(&self, writer: &mut impl Write) -> Result<(), fmt::Error>;
}

/// This is used for a regular [Derivation], where we refer to other derivations via their store
/// paths (to their drv files).
impl WriteDrvReference for StorePath {
    fn write_drv_reference(&self, writer: &mut impl Write) -> Result<(), fmt::Error> {
        writer.write_str(self.to_absolute_path().as_str())
    }
}

/// This is used for a [PreDerivation]
///
/// This is the [NixHash]'s HEXLOWER digest. This is not the
/// [NixHash::to_nix_hash_string], but without the sha256: prefix).
impl WriteDrvReference for NixHash {
    fn write_drv_reference(&self, writer: &mut impl Write) -> Result<(), fmt::Error> {
        writer.write_str(&data_encoding::HEXLOWER.encode(&self.digest))
    }
}

pub fn write_input_derivations<DrvRef: WriteDrvReference>(
    writer: &mut impl Write,
    input_derivations: &BTreeMap<DrvRef, BTreeSet<String>>,
) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_char(BRACKET_OPEN)?;

    for (ii, (input_derivation_path, input_derivation)) in input_derivations.iter().enumerate() {
        if ii > 0 {
            writer.write_char(COMMA)?;
        }

        writer.write_char(PAREN_OPEN)?;
        writer.write_char(QUOTE)?;
        input_derivation_path.write_drv_reference(writer)?;
        writer.write_char(QUOTE)?;
        writer.write_char(COMMA)?;

        write_array_elements(
            writer,
            true,
            &BRACKET_OPEN.to_string(),
            &BRACKET_CLOSE.to_string(),
            input_derivation.iter().map(|s| &**s).collect(),
        )?;

        writer.write_char(PAREN_CLOSE)?;
    }

    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}

pub fn write_input_sources(
    writer: &mut impl Write,
    input_sources: &BTreeSet<StorePath>,
) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;

    write_array_elements(
        writer,
        true,
        &BRACKET_OPEN.to_string(),
        &BRACKET_CLOSE.to_string(),
        input_sources.iter().map(|s| s.to_absolute_path()).collect(),
    )?;

    Ok(())
}

pub fn write_system(writer: &mut impl Write, platform: &str) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_str(escape_string(platform).as_str())?;
    Ok(())
}

pub fn write_builder(writer: &mut impl Write, builder: &str) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    writer.write_str(escape_string(builder).as_str())?;
    Ok(())
}
pub fn write_arguments(writer: &mut impl Write, arguments: &[String]) -> Result<(), fmt::Error> {
    writer.write_char(COMMA)?;
    write_array_elements(
        writer,
        true,
        &BRACKET_OPEN.to_string(),
        &BRACKET_CLOSE.to_string(),
        arguments.iter().map(|s| &**s).collect(),
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

        write_array_elements(
            writer,
            false,
            &PAREN_OPEN.to_string(),
            &PAREN_CLOSE.to_string(),
            vec![&escape_string(key), &escape_string(environment)],
        )?;
    }

    writer.write_char(BRACKET_CLOSE)?;

    Ok(())
}
