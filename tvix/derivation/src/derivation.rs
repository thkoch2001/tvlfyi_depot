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

        write::append_outputs(&mut buffer, &self.outputs);
        write::append_input_derivations(&mut buffer, &self.input_derivations);
        write::append_input_sources(&mut buffer, &self.input_sources);
        write::append_platfrom(&mut buffer, &self.platform);
        write::append_builder(&mut buffer, &self.builder);
        write::append_arguments(&mut buffer, &self.arguments);
        write::append_enviroment(&mut buffer, &self.environment);

        buffer.push_str(write::PAREN_CLOSE);

        buffer
    }
}
