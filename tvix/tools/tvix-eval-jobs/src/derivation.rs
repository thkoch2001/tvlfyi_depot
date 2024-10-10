use nix_compat::store_path::StorePath;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Serialize)]
pub struct DerivationInfo {
    name: String,
    attr_path: String,
    drv_path: StorePath<String>,
    out_path: StorePath<String>,
    meta: Option<HashMap<String, serde_json::Value>>,
}

impl DerivationInfo {
    pub fn new(
        attr_path: String,
        drv_path: StorePath<String>,
        out_path: StorePath<String>,
    ) -> Self {
        Self {
            attr_path,
            name: out_path.name().to_string(),
            drv_path,
            out_path,
            meta: None,
        }
    }
}
