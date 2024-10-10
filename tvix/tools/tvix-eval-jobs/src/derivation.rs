use color_eyre::{eyre::eyre, Result};
use nix_compat::store_path::StorePath;
use serde::Serialize;
use std::collections::HashMap;
use tvix_eval::Value;

#[derive(Debug, Serialize)]
pub struct DerivationInfo {
    name: String,
    attr_path: String,
    drv_path: StorePath<String>,
    out_path: StorePath<String>,
    meta: Option<HashMap<String, serde_json::Value>>,
}

impl DerivationInfo {
    pub fn new(attr_path: String, name: Value, drv_path: Value, out_path: Value) -> Result<Self> {
        Ok(Self {
            attr_path,
            // TODO: Find a better way to get rid of double quotes.
            // Maybe use a different string type?
            name: name.to_string().trim_matches('"').to_string(),
            drv_path: StorePath::from_absolute_path(
                &drv_path
                    .to_contextful_str()
                    .map_err(|e| eyre!(e.to_string()))?,
            )?,
            out_path: StorePath::from_absolute_path(
                &out_path
                    .to_contextful_str()
                    .map_err(|e| eyre!(e.to_string()))?,
            )?,
            meta: None,
        })
    }
}
