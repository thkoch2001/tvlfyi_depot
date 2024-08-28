//! Contains builtins that fetch paths from the Internet, or local filesystem.

use super::utils::select_string;
use crate::{
    fetchers::{url_basename, Fetch},
    tvix_store_io::TvixStoreIO,
};
use nix_compat::nixhash;
use std::rc::Rc;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::generators::Gen;
use tvix_eval::generators::GenCo;
use tvix_eval::{CatchableErrorKind, ErrorKind, NixAttrs, Value};
use url::Url;

// Used as a return type for extract_fetch_args, which is sharing some
// parsing code between the fetchurl and fetchTarball builtins.
struct NixFetchArgs {
    url: Url,
    name: Option<String>,
    sha256: Option<[u8; 32]>,
}

// `fetchurl` and `fetchTarball` accept a single argument, which can either be the URL (as string),
// or an attrset, where `url`, `sha256` and `name` keys are allowed.
async fn extract_fetch_args(
    co: &GenCo,
    args: Value,
) -> Result<Result<NixFetchArgs, CatchableErrorKind>, ErrorKind> {
    if let Ok(url_str) = args.to_str() {
        // Get the raw bytes, not the ToString repr.
        let url_str =
            String::from_utf8(url_str.as_bytes().to_vec()).map_err(|_| ErrorKind::Utf8)?;

        // Parse the URL.
        let url = Url::parse(&url_str).map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

        return Ok(Ok(NixFetchArgs {
            url,
            name: None,
            sha256: None,
        }));
    }

    let attrs = args.to_attrs().map_err(|_| ErrorKind::TypeError {
        expected: "attribute set or contextless string",
        actual: args.type_of(),
    })?;

    let url_str = match select_string(co, &attrs, "url").await? {
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

    // Disallow other attrset keys, to match Nix' behaviour.
    // We complain about the first unexpected key we find in the list.
    const VALID_KEYS: [&[u8]; 3] = [b"url", b"name", b"sha256"];
    if let Some(first_invalid_key) = attrs.keys().find(|k| !&VALID_KEYS.contains(&k.as_bytes())) {
        return Err(ErrorKind::UnexpectedArgumentBuiltin(
            first_invalid_key.clone(),
        ));
    }

    // parse the sha256 string into a digest.
    let sha256 = match sha256_str {
        Some(sha256_str) => {
            let nixhash = nixhash::from_str(&sha256_str, Some("sha256"))
                // TODO: DerivationError::InvalidOutputHash should be moved to ErrorKind::InvalidHash and used here instead
                .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

            Some(nixhash.digest_as_bytes().try_into().expect("is sha256"))
        }
        None => None,
    };

    // Parse the URL.
    let url = Url::parse(&url_str).map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

    Ok(Ok(NixFetchArgs { url, name, sha256 }))
}

enum NixFetchTreeType {
    File {
        url: Url,
    },
    Tarball {
        url: Url,
    },
    Git {
        url: Url,
        reference: Option<String>,
        revision: Option<String>,
        shallow: bool,
        submodules: bool,
        all_references: bool,
        // TODO(Shvedov): Support more
    },
    // TODO(Shvedov): Support other types
}

struct NixFetchTreeArgs {
    fetch_type: NixFetchTreeType,
    nar_hash: Option<[u8; 32]>,
}

async fn extract_fetch_tree_args(
    co: &GenCo,
    args: Value,
) -> Result<Result<NixFetchTreeArgs, CatchableErrorKind>, ErrorKind> {
    let attrs = args.to_attrs().map_err(|_| ErrorKind::TypeError {
        expected: "attribute set",
        actual: args.type_of(),
    })?;

    // TODO(Shvedov): Get rid of copy-pastes with extract_fetch

    let url_str = match select_string(&co, &attrs, "url").await? {
        Ok(s) => s.ok_or_else(|| ErrorKind::AttributeNotFound { name: "url".into() })?,
        Err(cek) => return Ok(Err(cek)),
    };
    let type_str = match select_string(co, &attrs, "type").await? {
        Ok(s) => s.ok_or_else(|| ErrorKind::AttributeNotFound {
            name: "type".into(),
        })?,
        Err(cek) => return Ok(Err(cek)),
    };
    let nar_hash_str = match select_string(co, &attrs, "narHash").await? {
        Ok(s) => s,
        Err(cek) => return Ok(Err(cek)),
    };

    // parse the sha256 string into a digest.
    let nar_hash = match nar_hash_str {
        Some(sha256_str) => {
            let nixhash = nixhash::from_str(&sha256_str, Some("sha256"))
                // TODO: DerivationError::InvalidOutputHash should be moved to ErrorKind::InvalidHash and used here instead
                .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

            Some(nixhash.digest_as_bytes().try_into().expect("is sha256"))
        }
        None => None,
    };

    let url = Url::parse(&url_str).map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;
    let fetch_type = match type_str.as_str() {
        "file" => Ok(NixFetchTreeType::File { url }),
        "tarball" => Ok(NixFetchTreeType::Tarball { url }),
        "git" => Ok(NixFetchTreeType::Git {
            url,
            // TODO(Shvedov): Parse others
            reference: None,
            revision: None,
            shallow: true,
            submodules: false,
            all_references: false,
        }),
        _ => Err(ErrorKind::InvalidAttributeName(type_str.into())),
    }?;

    Ok(Ok(NixFetchTreeArgs {
        fetch_type,
        nar_hash,
    }))
}

#[allow(unused_variables)] // for the `state` arg, for now
#[builtins(state = "Rc<TvixStoreIO>")]
pub(crate) mod fetcher_builtins {
    use nix_compat::nixhash::NixHash;

    use super::*;

    /// Consumes a fetch.
    /// If there is enough info to calculate the store path without fetching,
    /// queue the fetch to be fetched lazily, and return the store path.
    /// If there's not enough info to calculate it, do the fetch now, and then
    /// return the store path as String.
    fn fetch_lazy_str(
        state: Rc<TvixStoreIO>,
        name: String,
        fetch: Fetch,
    ) -> Result<String, ErrorKind> {
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
                Ok(store_path.to_absolute_path())
            }
            None => {
                // If we don't have enough info, do the fetch now.
                let (store_path, _root_node) = state
                    .tokio_handle
                    .block_on(async { state.fetcher.ingest_and_persist(&name, fetch).await })
                    .map_err(|e| ErrorKind::TvixError(Rc::new(e)))?;

                Ok(store_path.to_absolute_path())
            }
        }
    }

    /// Same as fetch_lazy, but returns Value::Path
    fn fetch_lazy(state: Rc<TvixStoreIO>, name: String, fetch: Fetch) -> Result<Value, ErrorKind> {
        let path_str = fetch_lazy_str(state, name, fetch)?;
        Ok(Value::Path(Box::new(path_str.into())))
    }

    #[builtin("fetchurl")]
    async fn builtin_fetchurl(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let args = match extract_fetch_args(&co, args).await? {
            Ok(args) => args,
            Err(cek) => return Ok(Value::from(cek)),
        };

        // Derive the name from the URL basename if not set explicitly.
        let name = args
            .name
            .unwrap_or_else(|| url_basename(&args.url).to_owned());

        fetch_lazy(
            state,
            name,
            Fetch::URL {
                url: args.url,
                exp_hash: args.sha256.map(NixHash::Sha256),
            },
        )
    }

    #[builtin("fetchTarball")]
    async fn builtin_fetch_tarball(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let args = match extract_fetch_args(&co, args).await? {
            Ok(args) => args,
            Err(cek) => return Ok(Value::from(cek)),
        };

        // Name defaults to "source" if not set explicitly.
        const DEFAULT_NAME_FETCH_TARBALL: &str = "source";
        let name = args
            .name
            .unwrap_or_else(|| DEFAULT_NAME_FETCH_TARBALL.to_owned());

        fetch_lazy(
            state,
            name,
            Fetch::Tarball {
                url: args.url,
                exp_nar_sha256: args.sha256,
            },
        )
    }

    #[builtin("fetchGit")]
    async fn builtin_fetch_git(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        Err(ErrorKind::NotImplemented("fetchGit"))
    }

    #[builtin("fetchTree")]
    async fn builtin_fetch_tree(
        state: Rc<TvixStoreIO>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind> {
        let args = match extract_fetch_tree_args(&co, args).await? {
            Ok(args) => args,
            Err(cek) => return Ok(Value::from(cek)),
        };

        let path = fetch_lazy_str(
            state,
            "source".into(),
            match args.fetch_type {
                NixFetchTreeType::File { url } => Fetch::URL {
                    url,
                    exp_hash: args.nar_hash.map(NixHash::Sha256),
                },
                NixFetchTreeType::Tarball { url } => Fetch::Tarball {
                    url,
                    exp_nar_sha256: args.nar_hash,
                },
                NixFetchTreeType::Git {
                    url,
                    revision,
                    reference,
                    shallow,
                    submodules,
                    all_references,
                } => Fetch::Git {
                    url,
                    revision,
                    reference,
                    shallow,
                    submodules,
                    all_references,

                    exp_hash: args.nar_hash.map(NixHash::Sha256),
                },
            },
        )?;

        // TODO(Shvedov): Use thunk here to gather things lazyly:
        // Value::suspended_native_thunk(Box::new(move || { .. }))
        // The problem for me here was to get the StorState inside closure.
        Ok(Value::attrs(NixAttrs::from_iter([
            ("outPath", Value::String(path.into())),
            // TODO(Shvedov): Implement filling all other and this values correctly
            ("lastModified", 0.into()),
            ("lastModifiedDate", Value::String("197001010000".into())),
            ("narHash", Value::String("sha256-TODO".into())),
        ])))
    }
}
