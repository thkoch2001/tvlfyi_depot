use std::collections::HashSet;
use serde_derive::Deserialize; // TODO(tazjin): move away from serde_derive

#[derive(Deserialize)]
pub struct Config {
    pub database_url: String,
    pub irc_config_path: String,
    #[serde(default)]
    pub admins: HashSet<String>,
    #[serde(default)]
    pub log_filter: Option<String>,
}
