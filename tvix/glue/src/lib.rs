pub mod builtins;
pub mod fetchers;
pub mod known_paths;
pub mod refscan;
pub mod tvix_build;
pub mod tvix_io;
pub mod tvix_store_io;

mod fetchurl;

#[cfg(test)]
mod tests;

/// Tell the Evaluator to resolve `<nix>` to the path `/__corepkgs__`,
/// which has special handling in [tvix_io::TvixIO].
/// This is used in nixpkgs to import `fetchurl.nix` from `<nix>`.
pub fn configure_nix_path<'co, 'ro, 'env, IO>(
    eval_builder: tvix_eval::EvaluatorBuilder<'co, 'ro, 'env, IO>,
    nix_search_path: &Option<String>,
) -> tvix_eval::EvaluatorBuilder<'co, 'ro, 'env, IO> {
    eval_builder.nix_path(
        nix_search_path
            .as_ref()
            .map(|p| format!("nix=/__corepkgs__:{}", p))
            .or_else(|| Some("nix=/__corepkgs__".to_string())),
    )
}
