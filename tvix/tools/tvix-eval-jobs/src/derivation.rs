use bstr::ByteSlice;
use serde::Serialize;
use std::collections::HashMap;
use tvix_eval::Value;

#[derive(Debug, Serialize)]
pub struct DerivationInfo {
    name: String,
    attr_path: String,
    drv_path: String,
    out_path: String,
    meta: Option<HashMap<String, serde_json::Value>>,
}

impl DerivationInfo {
    pub fn new(attr_path: String, name: Value, drv_path: Value, out_path: Value) -> Self {
        Self {
            attr_path,
            name: name
                .to_str()
                .expect("Did not find a name")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            drv_path: drv_path
                .to_contextful_str()
                .expect("Did not find a string derivation path")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            out_path: out_path
                .to_contextful_str()
                .expect("Did not find a string output path")
                .as_bytes()
                .to_str_lossy()
                .to_string(),
            meta: None,
        }
    }
}
