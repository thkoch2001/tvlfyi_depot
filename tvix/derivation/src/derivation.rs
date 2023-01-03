use crate::output::Output;
use crate::write;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

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

impl ToString for Derivation {
    fn to_string(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(write::DERIVATION_PREFIX);
        buffer.push_str(write::PAREN_OPEN);

        buffer.push_str(&write::write_outputs(&self.outputs));
        buffer.push_str(&write::write_input_derivations(&self.input_derivations));
        buffer.push_str(&write::write_input_sources(&self.input_sources));
        buffer.push_str(&write::write_platfrom(&self.platform));
        buffer.push_str(&write::write_builder(&self.builder));
        buffer.push_str(&write::write_arguments(&self.arguments));
        buffer.push_str(&write::write_enviroment(&self.environment));

        buffer.push_str(write::PAREN_CLOSE);

        buffer
    }
}
