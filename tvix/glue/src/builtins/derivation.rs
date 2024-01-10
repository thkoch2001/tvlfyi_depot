//! Implements `builtins.derivation`, the core of what makes Nix build packages.
use crate::builtins::DerivationError;
use crate::known_paths::KnownPaths;
use bstr::BString;
use futures::pin_mut;
use nix_compat::derivation::{Derivation, Output};
use nix_compat::nixhash;
use std::cell::RefCell;
use std::collections::{btree_map, BTreeSet};
use std::path::Path;
use std::rc::Rc;
use tvix_castore::blobservice::BlobService;
use tvix_castore::directoryservice::DirectoryService;
use tvix_eval::builtin_macros::builtins;
use tvix_eval::generators::{self, emit_warning_kind, GenCo};
use tvix_eval::{
    AddContext, CatchableErrorKind, CoercionKind, ErrorKind, NixAttrs, NixContext,
    NixContextElement, Value, WarningKind,
};
use tvix_store::pathinfoservice::PathInfoService;

// Constants used for strangely named fields in derivation inputs.
const STRUCTURED_ATTRS: &str = "__structuredAttrs";
const IGNORE_NULLS: &str = "__ignoreNulls";

/// Populate the inputs of a derivation from the build references
/// found when scanning the derivation's parameters and extracting their contexts.
fn populate_inputs(drv: &mut Derivation, full_context: NixContext) {
    for element in full_context.iter() {
        match element {
            NixContextElement::Plain(source) => {
                drv.input_sources.insert(source.clone());
            }

            NixContextElement::Single { name, derivation } => {
                match drv.input_derivations.entry(derivation.clone()) {
                    btree_map::Entry::Vacant(entry) => {
                        entry.insert(BTreeSet::from([name.clone()]));
                    }

                    btree_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().insert(name.clone());
                    }
                }
            }

            NixContextElement::Derivation(_drv_path) => {
                // This is a hard one, it means that
                // we are depending on a drvPath of ourselves
                // *or* another derivation's drvPath.
                // What to do here?
                panic!("please do not depend on drvPath, I have 2 hours of sleep in blood");
            }
        }
    }
}

/// Populate the output configuration of a derivation based on the
/// parameters passed to the call, configuring a fixed-output derivation output
/// if necessary.
///
/// This function handles all possible combinations of the
/// parameters, including invalid ones.
///
/// Due to the support for SRI hashes, and how these are passed along to
/// builtins.derivation, outputHash and outputHashAlgo can have values which
/// need to be further modified before constructing the Derivation struct.
///
/// If outputHashAlgo is an SRI hash, outputHashAlgo must either be an empty
/// string, or the hash algorithm as specified in the (single) SRI (entry).
/// SRI strings with multiple hash algorithms are not supported.
///
/// In case an SRI string was used, the (single) fixed output is populated
/// with the hash algo name, and the hash digest is populated with the
/// (lowercase) hex encoding of the digest.
///
/// These values are only rewritten for the outputs, not what's passed to env.
///
/// The return value may optionally contain a warning.
fn handle_fixed_output(
    drv: &mut Derivation,
    hash_str: Option<String>,      // in nix: outputHash
    hash_algo_str: Option<String>, // in nix: outputHashAlgo
    hash_mode_str: Option<String>, // in nix: outputHashmode
) -> Result<Option<WarningKind>, ErrorKind> {
    // If outputHash is provided, ensure hash_algo_str is compatible.
    // If outputHash is not provided, do nothing.
    if let Some(hash_str) = hash_str {
        // treat an empty algo as None
        let hash_algo_str = match hash_algo_str {
            Some(s) if s.is_empty() => None,
            Some(s) => Some(s),
            None => None,
        };

        // construct a NixHash.
        let nixhash = nixhash::from_str(&hash_str, hash_algo_str.as_deref())
            .map_err(DerivationError::InvalidOutputHash)?;
        let algo = nixhash.algo();

        // construct the fixed output.
        drv.outputs.insert(
            "out".to_string(),
            Output {
                path: "".to_string(),
                ca_hash: match hash_mode_str.as_deref() {
                    None | Some("flat") => Some(nixhash::CAHash::Flat(nixhash)),
                    Some("recursive") => Some(nixhash::CAHash::Nar(nixhash)),
                    Some(other) => {
                        return Err(DerivationError::InvalidOutputHashMode(other.to_string()))?
                    }
                },
            },
        );

        // Peek at hash_str once more.
        // If it was a SRI hash, but is not using the correct length, this means
        // the padding was wrong. Emit a warning in that case.
        let sri_prefix = format!("{}-", algo);
        if let Some(rest) = hash_str.strip_prefix(&sri_prefix) {
            if data_encoding::BASE64.encode_len(algo.digest_length()) != rest.len() {
                return Ok(Some(WarningKind::SRIHashWrongPadding));
            }
        }
    }
    Ok(None)
}

/// This structure contains all state regarding derivation bookkeeping, e.g.
/// hash derivation modulo and contains a direct accessor to a [`TvixStoreIO`]
/// and a ['tokio::runtime::Handle`].
///
/// At this point, you may ask yourself, why not reusing [`tvix_eval::EvalIO`] interface and
/// make the glue agnostic of the underlying store implementation?
///
/// Unfortunately, due to various limitations surrounding async traits (even in Rust ≥ 1.75.0)
/// and a pragmatic approach, we will prefer a tight coupling with a direct access to
/// the underlying implementations we need. After all, this crate is a glue, so it's expected
/// to make it tightly coupled about a specific implementation.
///
/// In the future, we may revisit and figure out how to generalize this interface and hide this
/// implementation detail of the glue itself so that glue can be used with more than one
/// implementation of store.
pub struct DerivationBuiltinState<BS, DS, PS> {
    known_paths: RefCell<KnownPaths>,
    // For now, we don't use it, but the objective is to build all the machinery
    // for `builtins.filterSource` and `builtins.path` with it.
    #[allow(dead_code)]
    store: crate::tvix_store_io::TvixStoreIO<BS, DS, PS>,
}

impl<BS, DS, PS> DerivationBuiltinState<BS, DS, PS> {
    pub fn new(store: crate::tvix_store_io::TvixStoreIO<BS, DS, PS>) -> Self {
        Self {
            known_paths: Default::default(),
            store,
        }
    }
}

type InternalDerivationBuiltinState<BS, DS, PS> = Rc<DerivationBuiltinState<BS, DS, PS>>;

async fn filter_and_import<BS, DS, PS>(
    state: InternalDerivationBuiltinState<BS, DS, PS>,
    co: GenCo,
    name: &str,
    path: &Path,
    filter: Option<&Value>,
) -> Result<
    (
        nix_compat::store_path::StorePath,
        tvix_castore::proto::node::Node,
    ),
    ErrorKind,
>
where
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
    PS: AsRef<dyn PathInfoService>,
{
    let mut entries_per_depths: Vec<Vec<walkdir::DirEntry>> = vec![Vec::new()];
    let mut it = walkdir::WalkDir::new(path)
        .follow_links(false)
        .follow_root_links(false)
        .contents_first(false)
        .sort_by_file_name()
        .into_iter();

    // Skip root node.
    entries_per_depths[0].push(
        it.next()
            .expect("Failed to obtain root node")
            .expect("Failed to read root node"),
    );

    while let Some(entry) = it.next() {
        // Entry could be a NotFound, if the root path specified does not exist.
        let entry = entry.expect("Failed to find the entry");

        let file_type = if entry.file_type().is_dir() {
            "directory"
        } else if entry.file_type().is_file() {
            "file"
        } else if entry.file_type().is_symlink() {
            "symlink"
        } else {
            "other"
        };

        let should_keep: bool = if let Some(filter) = filter {
            generators::request_force(
                &co,
                generators::request_call_with(
                    &co,
                    filter.clone(),
                    [
                        Value::String(entry.path().to_string_lossy().as_ref().into()),
                        Value::String(file_type.into()),
                    ],
                )
                .await,
            )
            .await
            .as_bool()?
        } else {
            true
        };

        if !should_keep {
            println!("Skipping {:?}...", entry);
            if file_type == "directory" {
                it.skip_current_dir();
            }
            continue;
        } else {
            println!("Keeping {:?}...", entry);
        }

        if entry.depth() >= entries_per_depths.len() {
            debug_assert!(
                entry.depth() == entries_per_depths.len(),
                "We should not be allocating more than one level at once, requested node at depth {}, had entries per depth containing {} levels",
                entry.depth(),
                entries_per_depths.len()
            );

            entries_per_depths.push(vec![entry]);
        } else {
            entries_per_depths[entry.depth()].push(entry);
        }

        // FUTUREWORK: determine when it's the right moment to flush a level to the ingester.
    }

    let entries_stream = tvix_castore::import::leveled_entries_to_stream(entries_per_depths);

    pin_mut!(entries_stream);

    let root_node = state
        .store
        .ingest_entries_sync(entries_stream)
        .expect("Failed to ingest entries");

    let that_root_node = root_node.clone();
    let spath = state
        .store
        .import_root_node_sync(path, name, that_root_node)
        .expect("Failed to import root node");

    Ok((spath, root_node))
}

#[builtins(state(
    "Rc<DerivationBuiltinState<BS, DS, PS>>",
    "BS: 'static, DS: 'static, PS: 'static",
    "BS: AsRef<dyn BlobService> + Clone, DS: AsRef<dyn DirectoryService>, PS: AsRef<dyn PathInfoService>"
))]
pub(crate) mod derivation_builtins {
    use std::collections::BTreeMap;

    use super::*;
    use nix_compat::store_path::hash_placeholder;
    use sha2::{Digest, Sha256};
    use tvix_eval::generators::Gen;
    use tvix_eval::{NixContext, NixContextElement, NixString};
    use tvix_store::pathinfoservice::PathInfoService;

    #[builtin("placeholder")]
    async fn builtin_placeholder(co: GenCo, input: Value) -> Result<Value, ErrorKind> {
        if input.is_catchable() {
            return Ok(input);
        }

        let placeholder = hash_placeholder(
            input
                .to_str()
                .context("looking at output name in builtins.placeholder")?
                .as_str(),
        );

        Ok(placeholder.into())
    }

    /// Strictly construct a Nix derivation from the supplied arguments.
    ///
    /// This is considered an internal function, users usually want to
    /// use the higher-level `builtins.derivation` instead.
    #[builtin("derivationStrict")]
    async fn builtin_derivation_strict<BS, DS, PS>(
        state: InternalDerivationBuiltinState<BS, DS, PS>,
        co: GenCo,
        input: Value,
    ) -> Result<Value, ErrorKind> {
        if input.is_catchable() {
            return Ok(input);
        }

        let input = input.to_attrs()?;
        let name = generators::request_force(&co, input.select_required("name")?.clone()).await;

        if name.is_catchable() {
            return Ok(name);
        }

        let name = name.to_str().context("determining derivation name")?;

        if name.is_empty() {
            return Err(ErrorKind::Abort("derivation has empty name".to_string()));
        }

        let mut drv = Derivation::default();
        drv.outputs.insert("out".to_string(), Default::default());
        let mut input_context = NixContext::new();

        #[inline]
        async fn strong_importing_coerce_to_string(
            co: &GenCo,
            val: Value,
        ) -> Result<NixString, CatchableErrorKind> {
            let val = generators::request_force(co, val).await;
            match generators::request_string_coerce(
                co,
                val,
                CoercionKind {
                    strong: true,
                    import_paths: true,
                },
            )
            .await
            {
                Err(cek) => Err(cek),
                Ok(val_str) => Ok(val_str),
            }
        }

        /// Inserts a key and value into the drv.environment BTreeMap, and fails if the
        /// key did already exist before.
        fn insert_env(drv: &mut Derivation, k: &str, v: BString) -> Result<(), DerivationError> {
            if drv.environment.insert(k.into(), v).is_some() {
                return Err(DerivationError::DuplicateEnvVar(k.into()));
            }
            Ok(())
        }

        // Check whether null attributes should be ignored or passed through.
        let ignore_nulls = match input.select(IGNORE_NULLS) {
            Some(b) => generators::request_force(&co, b.clone()).await.as_bool()?,
            None => false,
        };

        // peek at the STRUCTURED_ATTRS argument.
        // If it's set and true, provide a BTreeMap that gets populated while looking at the arguments.
        // We need it to be a BTreeMap, so iteration order of keys is reproducible.
        let mut structured_attrs: Option<BTreeMap<String, serde_json::Value>> =
            match input.select(STRUCTURED_ATTRS) {
                Some(b) => generators::request_force(&co, b.clone())
                    .await
                    .as_bool()?
                    .then_some(Default::default()),
                None => None,
            };

        // Look at the arguments passed to builtins.derivationStrict.
        // Some set special fields in the Derivation struct, some change
        // behaviour of other functionality.
        for (arg_name, arg_value) in input.clone().into_iter_sorted() {
            // force the current value.
            let value = generators::request_force(&co, arg_value).await;

            // filter out nulls if ignore_nulls is set.
            if ignore_nulls && matches!(value, Value::Null) {
                continue;
            }

            match arg_name.as_str() {
                // Command line arguments to the builder.
                // These are only set in drv.arguments.
                "args" => {
                    for arg in value.to_list()? {
                        match strong_importing_coerce_to_string(&co, arg).await {
                            Err(cek) => return Ok(Value::Catchable(cek)),
                            Ok(s) => {
                                input_context.mimic(&s);
                                drv.arguments.push(s.as_str().to_string())
                            }
                        }
                    }
                }

                // If outputs is set, remove the original default `out` output,
                // and replace it with the list of outputs.
                "outputs" => {
                    let outputs = value
                        .to_list()
                        .context("looking at the `outputs` parameter of the derivation")?;

                    // Remove the original default `out` output.
                    drv.outputs.clear();

                    let mut output_names = vec![];

                    for output in outputs {
                        let output_name = generators::request_force(&co, output)
                            .await
                            .to_str()
                            .context("determining output name")?;

                        input_context.mimic(&output_name);

                        // Populate drv.outputs
                        if drv
                            .outputs
                            .insert(output_name.as_str().to_string(), Default::default())
                            .is_some()
                        {
                            Err(DerivationError::DuplicateOutput(
                                output_name.as_str().into(),
                            ))?
                        }
                        output_names.push(output_name.as_str().to_string());
                    }

                    // Add drv.environment[outputs] unconditionally.
                    insert_env(&mut drv, arg_name.as_str(), output_names.join(" ").into())?;
                    // drv.environment[$output_name] is added after the loop,
                    // with whatever is in drv.outputs[$output_name].
                }

                // handle builder and system.
                "builder" | "system" => {
                    match strong_importing_coerce_to_string(&co, value).await {
                        Err(cek) => return Ok(Value::Catchable(cek)),
                        Ok(val_str) => {
                            input_context.mimic(&val_str);

                            if arg_name.as_str() == "builder" {
                                drv.builder = val_str.as_str().to_owned();
                            } else {
                                drv.system = val_str.as_str().to_owned();
                            }

                            // Either populate drv.environment or structured_attrs.
                            if let Some(ref mut structured_attrs) = structured_attrs {
                                // No need to check for dups, we only iterate over every attribute name once
                                structured_attrs
                                    .insert(arg_name.as_str().into(), val_str.as_str().into());
                            } else {
                                insert_env(&mut drv, arg_name.as_str(), val_str.as_bytes().into())?;
                            }
                        }
                    }
                }

                // Don't add STRUCTURED_ATTRS if enabled.
                STRUCTURED_ATTRS if structured_attrs.is_some() => continue,
                // IGNORE_NULLS is always skipped, even if it's not set to true.
                IGNORE_NULLS => continue,

                // all other args.
                _ => {
                    // In SA case, force and add to structured attrs.
                    // In non-SA case, coerce to string and add to env.
                    if let Some(ref mut structured_attrs) = structured_attrs {
                        let val = generators::request_force(&co, value).await;
                        if matches!(val, Value::Catchable(_)) {
                            return Ok(val);
                        }

                        // TODO(raitobezarius): context for json values?
                        // input_context.mimic(&val);

                        let val_json = match val.into_json(&co).await? {
                            Ok(v) => v,
                            Err(cek) => return Ok(Value::Catchable(cek)),
                        };

                        // No need to check for dups, we only iterate over every attribute name once
                        structured_attrs.insert(arg_name.as_str().to_string(), val_json);
                    } else {
                        match strong_importing_coerce_to_string(&co, value).await {
                            Err(cek) => return Ok(Value::Catchable(cek)),
                            Ok(val_str) => {
                                input_context.mimic(&val_str);

                                insert_env(&mut drv, arg_name.as_str(), val_str.as_bytes().into())?;
                            }
                        }
                    }
                }
            }
        }
        // end of per-argument loop

        // Configure fixed-output derivations if required.
        {
            async fn select_string(
                co: &GenCo,
                attrs: &NixAttrs,
                key: &str,
            ) -> Result<Result<Option<String>, CatchableErrorKind>, ErrorKind> {
                if let Some(attr) = attrs.select(key) {
                    match strong_importing_coerce_to_string(co, attr.clone()).await {
                        Err(cek) => return Ok(Err(cek)),
                        Ok(str) => return Ok(Ok(Some(str.as_str().to_string()))),
                    }
                }

                Ok(Ok(None))
            }

            let output_hash = match select_string(&co, &input, "outputHash")
                .await
                .context("evaluating the `outputHash` parameter")?
            {
                Err(cek) => return Ok(Value::Catchable(cek)),
                Ok(s) => s,
            };
            let output_hash_algo = match select_string(&co, &input, "outputHashAlgo")
                .await
                .context("evaluating the `outputHashAlgo` parameter")?
            {
                Err(cek) => return Ok(Value::Catchable(cek)),
                Ok(s) => s,
            };
            let output_hash_mode = match select_string(&co, &input, "outputHashMode")
                .await
                .context("evaluating the `outputHashMode` parameter")?
            {
                Err(cek) => return Ok(Value::Catchable(cek)),
                Ok(s) => s,
            };

            if let Some(warning) =
                handle_fixed_output(&mut drv, output_hash, output_hash_algo, output_hash_mode)?
            {
                emit_warning_kind(&co, warning).await;
            }
        }

        // Each output name needs to exist in the environment, at this
        // point initialised as an empty string, as the ATerm serialization of that is later
        // used for the output path calculation (which will also update output
        // paths post-calculation, both in drv.environment and drv.outputs)
        for output in drv.outputs.keys() {
            if drv
                .environment
                .insert(output.to_string(), String::new().into())
                .is_some()
            {
                emit_warning_kind(&co, WarningKind::ShadowedOutput(output.to_string())).await;
            }
        }

        if let Some(structured_attrs) = structured_attrs {
            // configure __json
            drv.environment.insert(
                "__json".to_string(),
                BString::from(serde_json::to_string(&structured_attrs)?),
            );
        }

        populate_inputs(&mut drv, input_context);
        let mut known_paths = state.known_paths.borrow_mut();

        // At this point, derivation fields are fully populated from
        // eval data structures.
        drv.validate(false)
            .map_err(DerivationError::InvalidDerivation)?;

        // Calculate the derivation_or_fod_hash for the current derivation.
        // This one is still intermediate (so not added to known_paths)
        let derivation_or_fod_hash_tmp =
            drv.derivation_or_fod_hash(|drv| known_paths.get_hash_derivation_modulo(drv));

        // Mutate the Derivation struct and set output paths
        drv.calculate_output_paths(&name, &derivation_or_fod_hash_tmp)
            .map_err(DerivationError::InvalidDerivation)?;

        let derivation_path = drv
            .calculate_derivation_path(&name)
            .map_err(DerivationError::InvalidDerivation)?;

        // recompute the hash derivation modulo and add to known_paths
        let derivation_or_fod_hash_final =
            drv.derivation_or_fod_hash(|drv| known_paths.get_hash_derivation_modulo(drv));

        known_paths.add_hash_derivation_modulo(
            derivation_path.to_absolute_path(),
            &derivation_or_fod_hash_final,
        );

        let mut new_attrs: Vec<(String, NixString)> = drv
            .outputs
            .into_iter()
            .map(|(name, output)| {
                (
                    name.clone(),
                    (
                        output.path,
                        Some(
                            NixContextElement::Single {
                                name,
                                derivation: derivation_path.to_absolute_path(),
                            }
                            .into(),
                        ),
                    )
                        .into(),
                )
            })
            .collect();

        new_attrs.push((
            "drvPath".to_string(),
            (
                derivation_path.to_absolute_path(),
                Some(NixContextElement::Derivation(derivation_path.to_absolute_path()).into()),
            )
                .into(),
        ));

        Ok(Value::Attrs(Box::new(NixAttrs::from_iter(
            new_attrs.into_iter(),
        ))))
    }

    #[builtin("toFile")]
    async fn builtin_to_file(co: GenCo, name: Value, content: Value) -> Result<Value, ErrorKind> {
        if name.is_catchable() {
            return Ok(name);
        }

        if content.is_catchable() {
            return Ok(content);
        }

        let name = name
            .to_str()
            .context("evaluating the `name` parameter of builtins.toFile")?;
        let content = content
            .to_contextful_str()
            .context("evaluating the `content` parameter of builtins.toFile")?;

        if content.iter_derivation().count() > 0 || content.iter_single_outputs().count() > 0 {
            return Err(ErrorKind::UnexpectedContext);
        }

        let path = nix_compat::store_path::build_text_path(
            name.as_str(),
            content.as_str(),
            content.iter_plain(),
        )
        .map_err(|_e| {
            nix_compat::derivation::DerivationError::InvalidOutputName(name.as_str().to_string())
        })
        .map_err(DerivationError::InvalidDerivation)?
        .to_absolute_path();

        let context: NixContext = NixContextElement::Plain(path.clone()).into();

        // TODO: actually persist the file in the store at that path ...

        Ok(Value::String(NixString::new_context_from(context, &path)))
    }

    #[builtin("path")]
    async fn builtin_path<BS: 'static, DS: 'static, PS: 'static>(
        state: InternalDerivationBuiltinState<BS, DS, PS>,
        co: GenCo,
        args: Value,
    ) -> Result<Value, ErrorKind>
    where
        BS: AsRef<dyn BlobService> + Clone,
        DS: AsRef<dyn DirectoryService>,
        PS: AsRef<dyn PathInfoService>,
    {
        let args = args.to_attrs()?;
        let path = args.select_required("path")?;
        let path = generators::request_force(&co, path.clone())
            .await
            .to_path()?;
        let name: String = if let Some(name) = args.select("name") {
            generators::request_force(&co, name.clone())
                .await
                .to_str()?
                .as_str()
                .to_string()
        } else {
            tvix_store::import::path_to_name(&path)
                .expect("Failed to derive the default name out of the path")
                .to_string()
        };
        let filter = args.select("filter");
        let recursive_ingestion = args
            .select("recursive")
            .map(|r| r.as_bool())
            .transpose()?
            .unwrap_or(true); // Yes, yes, Nix, by default, puts `recursive = true;`.
        let expected_sha256 = args
            .select("sha256")
            .map(|h| {
                h.to_str().and_then(|expected| {
                    nix_compat::nixhash::from_str(expected.as_ref(), None).map_err(|_err| {
                        // TODO: a better error would be nice, what do we use for wrong hashes?
                        ErrorKind::TypeError {
                            expected: "sha256",
                            actual: "not a sha256",
                        }
                    })
                })
            })
            .transpose()?;

        if !recursive_ingestion {
            return Ok(Value::Catchable(CatchableErrorKind::UnimplementedFeature(
                "flat `path`".to_string(),
            )));
        }

        let (spath, root_node) =
            filter_and_import(state.clone(), co, name.as_ref(), path.as_ref(), filter).await?;

        if let Some(expected_sha256) = expected_sha256 {
            let nar_hash: [u8; 32] = if recursive_ingestion {
                state
                    .store
                    .calculate_nar_sync(&root_node)
                    .expect("Failed to compute the recursive NAR hash representation")
                    .1
            } else {
                let contents = std::fs::read(&*path).expect("Failed to read file");
                let hash = Sha256::digest(contents);
                hash.into()
            };

            if &nar_hash[..] != expected_sha256.digest_as_bytes() {
                // TODO: propagate the root path
                return Err(ErrorKind::StorePathMismatchDuringImport);
            }
        }

        Ok(spath.to_absolute_path().into())
    }

    #[builtin("filterSource")]
    async fn builtin_filter_source<BS: 'static, DS: 'static, PS: 'static>(
        state: Rc<DerivationBuiltinState<BS, DS, PS>>,
        co: GenCo,
        #[lazy] filter: Value,
        path: Value,
    ) -> Result<Value, ErrorKind>
    where
        BS: AsRef<dyn BlobService> + Clone,
        DS: AsRef<dyn DirectoryService>,
        PS: AsRef<dyn PathInfoService>,
    {
        let p = path.to_path()?;
        let (spath, _) = filter_and_import(
            state,
            co,
            tvix_store::import::path_to_name(&p)
                .expect("Failed to derive the default name out of the path"),
            p.as_ref(),
            Some(&filter),
        )
        .await?;
        Ok(spath.to_absolute_path().into())
    }
}

pub use derivation_builtins::builtins as derivation_builtins;
