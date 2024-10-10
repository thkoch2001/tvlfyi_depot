use clap::Parser;
use std::path::PathBuf;

/// Provides a CLI interface to trigger mass evaluation jobs
///
/// Sets a default set of builtins similar to these present in Nix.
///
/// No `.drv` is written to the local `/nix/store` location.
///
/// The CLI interface is not stable and subject to change.
#[derive(Parser, Clone)]
pub(crate) struct Args {
    #[clap(long, short = 'E')]
    pub expr: Option<String>,

    /// Force recursion into attribute sets until derivations are encountered.
    /// This ignores any `recurseForDerivations` field.
    #[clap(long, default_value_t = false)]
    pub force_recurse: bool,

    /// Include the `meta` attribute of derivations
    #[clap(long, default_value_t = false)]
    pub include_meta: bool,

    /// Directory where derivation recipes (`.drv`) will be accumulated.
    #[clap(long)]
    pub derivation_directory: Option<PathBuf>,

    /// A colon-separated list of directories to use to resolve `<...>`-style paths
    #[clap(long, short = 'I', env = "NIX_PATH")]
    pub nix_search_path: Option<String>,

    #[clap(flatten)]
    pub service_addrs: tvix_store::utils::ServiceUrlsMemory,

    #[arg(long, env, default_value = "dummy://")]
    pub build_service_addr: String,
}
