use crate::nix_hash;
use crate::output::Output;
use crate::write;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, fmt, fmt::Write, iter::FromIterator};
use tvix_store::nixbase32::NIXBASE32;

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
    pub fn serialize(&self, writer: &mut impl Write) -> Result<(), fmt::Error> {
        writer.write_str(write::DERIVATION_PREFIX)?;
        writer.write_char(write::PAREN_OPEN)?;

        write::write_outputs(writer, &self.outputs)?;
        write::write_input_derivations(writer, &self.input_derivations)?;
        write::write_input_sources(writer, &self.input_sources)?;
        write::write_platfrom(writer, &self.platform)?;
        write::write_builder(writer, &self.builder)?;
        write::write_arguments(writer, &self.arguments)?;
        write::write_enviroment(writer, &self.environment)?;

        writer.write_char(write::PAREN_CLOSE)?;

        Ok(())
    }

    pub fn get_path(&self, name: &str) -> String {
        let mut hasher = Sha256::new();

        // step 1: input sources and derivations
        hasher.update(write::TEXT_COLON);

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
        hasher.update(write::SHA256_COLON);
        let mut derivation_hasher = Sha256::new();
        derivation_hasher.update(self.to_string());

        let aterm_digest = derivation_hasher.finalize();
        hasher.update(format!("{:x}", aterm_digest));
        hasher.update(write::COLON);

        // step 3: store path including name
        hasher.update(write::STORE_PATH_COLON);
        hasher.update(name);
        hasher.update(write::DOT_FILE_EXT);

        let aterm_digest = Vec::from_iter(hasher.finalize());
        let compressed = nix_hash::compress_hash(&aterm_digest, 20);

        format!(
            "{}-{}{}",
            NIXBASE32.encode(&compressed),
            name,
            write::DOT_FILE_EXT
        )
    }
}

impl ToString for Derivation {
    fn to_string(&self) -> String {
        let mut serialized_derivation = String::new();
        self.serialize(&mut serialized_derivation)
            .expect("Serialization failed.");
        serialized_derivation
    }
}
