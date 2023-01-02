use serde::{Deserialize, Serialize};
use tvix_store::nixpath::NixPath;

// This function is required by serde to deserialize files
// with missing keys.
fn default_resource() -> String {
    "".to_string()
}

#[derive(Serialize, Deserialize)]
pub struct Output {
    pub(crate) path: String,
    #[serde(default = "default_resource")]
    pub(crate) hash_algorithm: String,
    #[serde(default = "default_resource")]
    pub(crate) hash: String,
}

impl Output {
    pub fn is_fixed(&self) -> bool {
        self.hash_algorithm.is_empty()
    }

    pub fn validate(&self) -> anyhow::Result<()> {
        NixPath::from_string(&self.path)?;
        Ok(())
    }
}
