//! Contains builtins that fetch paths from the Internet

use super::utils::select_string;
use nix_compat::nixhash;
use std::rc::Rc;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::generators::GenCo;
use tvix_eval::{CatchableErrorKind, ErrorKind, Value};

// `fetchurl` and `fetchTarball` accept a single argument, which can either be the URL (as string),
// or an attrset, where `url`, `sha256` and `name` keys are allowed.
async fn extract_fetch_args(
    co: &GenCo,
    args: Value,
) -> Result<Result<(String, Option<String>, Option<[u8; 32]>), CatchableErrorKind>, ErrorKind> {
    if let Ok(url) = args.to_str() {
        return Ok(Ok((url.into(), None, None)));
    }

    let attrs = args.to_attrs().map_err(|_| ErrorKind::TypeError {
        expected: "attribute set or string",
        actual: args.type_of(),
    })?;

    let url = match select_string(co, &attrs, "url").await? {
        Ok(s) => s.ok_or_else(|| ErrorKind::AttributeNotFound { name: "url".into() })?,
        Err(cek) => return Ok(Err(cek)),
    };
    let name = match select_string(co, &attrs, "name").await? {
        Ok(s) => s,
        Err(cek) => return Ok(Err(cek)),
    };
    let sha256_str = match select_string(co, &attrs, "sha256").await? {
        Ok(s) => s,
        Err(cek) => return Ok(Err(cek)),
    };

    // TODO: disallow other attrset keys, to match Nix' behaviour.

    // parse the sha256 string into NixHash.
    let nix_hash = match sha256_str {
        Some(sha256_str) => {
            let nixhash = nixhash::from_str(&sha256_str, Some("sha256"))
                // TODO: DerivationError::InvalidOutputHash should be moved to ErrorKind::InvalidHash and used here instead
                .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

            Some(nixhash.digest_as_bytes().try_into().expect("is sha256"))
        }
        None => None,
    };

    Ok(Ok((url, name, nix_hash)))
}

#[allow(unused_variables)] // for the `state` arg, for now
#[builtins(state = "Rc<TvixStoreIO>")]
pub(crate) mod fetcher_builtins {
    use crate::{
        fetchers::{url_basename, Fetch},
        tvix_store_io::TvixStoreIO,
    };

    use super::*;

    use nix_compat::nixhash::NixHash;
    use tvix_eval::generators::Gen;

    /// Consumes a fetch.
    /// If there is enough info to calculate the store path without fetching,
    /// queue the fetch to be fetched lazily, and return the store path.
    /// If there's not enough info to calculate it, do the fetch now, and then
    /// return the store path.
    fn fetch_lazy(state: Rc<TvixStoreIO>, name: String, fetch: Fetch) -> Result<Value, ErrorKind> {
        match fetch
            .store_path(&name)
            .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?
        {
            Some(store_path) => {
                // Move the fetch to KnownPaths, so it can be actually fetched later.
                let sp = state
                    .known_paths
                    .borrow_mut()
                    .add_fetch(fetch, &name)
                    .expect("Tvix bug: should only fail if the store path cannot be calculated");

                debug_assert_eq!(
                    sp, store_path,
                    "calculated store path by KnownPaths should match"
                );

                // Emit the calculated Store Path.
                Ok(Value::Path(Box::new(store_path.to_absolute_path().into())))
            }
            None => {
                // If we don't have enough info, do the fetch now.
                info!(?fetch, "triggering required fetch");

                let (store_path, _root_node) = state
                    .tokio_handle
                    .block_on(async { state.fetcher.fetch_and_persist(&name, fetch).await })
                    .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

                Ok(Value::Path(Box::new(store_path.to_absolute_path().into())))
            }
        }
    }

    #[builtin("fetchurl")]
    async fn builtin_fetchurl(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let (url, name, exp_sha256) = match extract_fetch_args(&co, args).await? {
            Ok((url, name, exp_sha256)) => (url, name, exp_sha256),
            Err(cek) => return Ok(Value::from(cek)),
        };

        // Derive the name from the URL basename if not set explicitly.
        let name = name.unwrap_or_else(|| url_basename(&url).to_owned());

        fetch_lazy(
            state,
            name,
            Fetch::URL(url, exp_sha256.map(NixHash::Sha256)),
        )
    }

    #[builtin("fetchTarball")]
    async fn builtin_fetch_tarball(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let (url, name, exp_sha256) = match extract_fetch_args(&co, args).await? {
            Ok((url, name, nix_hash)) => (url, name, nix_hash),
            Err(cek) => return Ok(Value::from(cek)),
        };

        // Name defaults to "source" if not set explicitly.
        let name = name.unwrap_or_else(|| "source".to_owned());

        fetch_lazy(state, name, Fetch::Tarball(url, exp_sha256))
    }

    #[builtin("fetchGit")]
    async fn builtin_fetch_git(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        Err(ErrorKind::NotImplemented("fetchGit"))
    }
}
