//! This module provides an implementation of EvalIO talking to tvix-store.
use futures::{StreamExt, TryStreamExt};
use nix_compat::{nixhash::CAHash, store_path::StorePath};
use std::collections::BTreeMap;
use std::{
    cell::RefCell,
    io,
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio_util::io::SyncIoBridge;
use tracing::{error, instrument, warn, Level, Span};
use tracing_indicatif::span_ext::IndicatifSpanExt;
use tvix_build::buildservice::BuildService;
use tvix_eval::{EvalIO, FileType, StdIO};
use tvix_store::nar::NarCalculationService;

use tvix_castore::{
    blobservice::BlobService,
    directoryservice::{self, DirectoryService},
    Node,
};
use tvix_store::pathinfoservice::{PathInfo, PathInfoService};

use crate::fetchers::Fetcher;
use crate::known_paths::KnownPaths;
use crate::tvix_build::derivation_to_build_request;

/// Implements [EvalIO], asking given [PathInfoService], [DirectoryService]
/// and [BlobService].
///
/// In case the given path does not exist in these stores, we ask StdIO.
/// This is to both cover cases of syntactically valid store paths, that exist
/// on the filesystem (still managed by Nix), as well as being able to read
/// files outside store paths.
///
/// This structure is also directly used by the derivation builtins
/// and tightly coupled to it.
///
/// In the future, we may revisit that coupling and figure out how to generalize this interface and
/// hide this implementation detail of the glue itself so that glue can be used with more than one
/// implementation of "Tvix Store IO" which does not necessarily bring the concept of blob service,
/// directory service or path info service.
pub struct TvixStoreIO {
    // This is public so helper functions can interact with the stores directly.
    pub(crate) blob_service: Arc<dyn BlobService>,
    pub(crate) directory_service: Arc<dyn DirectoryService>,
    pub(crate) path_info_service: Arc<dyn PathInfoService>,
    pub(crate) nar_calculation_service: Arc<dyn NarCalculationService>,

    std_io: StdIO,
    #[allow(dead_code)]
    build_service: Arc<dyn BuildService>,
    pub(crate) tokio_handle: tokio::runtime::Handle,

    #[allow(clippy::type_complexity)]
    pub(crate) fetcher: Fetcher<
        Arc<dyn BlobService>,
        Arc<dyn DirectoryService>,
        Arc<dyn PathInfoService>,
        Arc<dyn NarCalculationService>,
    >,

    // Paths known how to produce, by building or fetching.
    pub known_paths: RefCell<KnownPaths>,
}

impl TvixStoreIO {
    pub fn new(
        blob_service: Arc<dyn BlobService>,
        directory_service: Arc<dyn DirectoryService>,
        path_info_service: Arc<dyn PathInfoService>,
        nar_calculation_service: Arc<dyn NarCalculationService>,
        build_service: Arc<dyn BuildService>,
        tokio_handle: tokio::runtime::Handle,
    ) -> Self {
        Self {
            blob_service: blob_service.clone(),
            directory_service: directory_service.clone(),
            path_info_service: path_info_service.clone(),
            nar_calculation_service: nar_calculation_service.clone(),
            std_io: StdIO {},
            build_service,
            tokio_handle,
            fetcher: Fetcher::new(
                blob_service,
                directory_service,
                path_info_service,
                nar_calculation_service,
            ),
            known_paths: Default::default(),
        }
    }

    /// for a given [StorePath] and additional [Path] inside the store path,
    /// look up the [PathInfo], and if it exists, and then use
    /// [directoryservice::descend_to] to return the
    /// [Node] specified by `sub_path`.
    ///
    /// In case there is no PathInfo yet, this means we need to build it
    /// (which currently is stubbed out still).
    #[instrument(skip(self, store_path), fields(store_path=%store_path, indicatif.pb_show=tracing::field::Empty), ret(level = Level::TRACE), err(level = Level::TRACE))]
    async fn store_path_to_node(
        &self,
        store_path: &StorePath<String>,
        sub_path: &Path,
    ) -> io::Result<Option<Node>> {
        // Find the root node for the store_path.
        // It asks the PathInfoService first, but in case there was a Derivation
        // produced that would build it, fall back to triggering the build.
        // To populate the input nodes, it might recursively trigger builds of
        // its dependencies too.
        let root_node = match self
            .path_info_service
            .as_ref()
            .get(*store_path.digest())
            .await?
        {
            // TODO: use stricter typed BuildRequest here.
            Some(path_info) => path_info.node,
            // If there's no PathInfo found, this normally means we have to
            // trigger the build (and insert into PathInfoService, after
            // reference scanning).
            // However, as Tvix is (currently) not managing /nix/store itself,
            // we return Ok(None) to let std_io take over.
            // While reading from store paths that are not known to Tvix during
            // that evaluation clearly is an impurity, we still need to support
            // it for things like <nixpkgs> pointing to a store path.
            // In the future, these things will (need to) have PathInfo.
            None => {
                // The store path doesn't exist yet, so we need to fetch or build it.
                // We check for fetches first, as we might have both native
                // fetchers and FODs in KnownPaths, and prefer the former.
                // This will also find [Fetch] synthesized from
                // `builtin:fetchurl` Derivations.
                let maybe_fetch = self
                    .known_paths
                    .borrow()
                    .get_fetch_for_output_path(store_path);

                match maybe_fetch {
                    Some((name, fetch)) => {
                        let (sp, root_node) = self
                            .fetcher
                            .ingest_and_persist(&name, fetch)
                            .await
                            .map_err(|e| {
                            std::io::Error::new(std::io::ErrorKind::InvalidData, e)
                        })?;

                        debug_assert_eq!(
                            sp.to_absolute_path(),
                            store_path.as_ref().to_absolute_path(),
                            "store path returned from fetcher must match store path we have in fetchers"
                        );

                        root_node
                    }
                    None => {
                        // Look up the derivation for this output path.
                        let (drv_path, drv) = {
                            let known_paths = self.known_paths.borrow();
                            match known_paths.get_drv_path_for_output_path(store_path) {
                                Some(drv_path) => (
                                    drv_path.to_owned(),
                                    known_paths.get_drv_by_drvpath(drv_path).unwrap().to_owned(),
                                ),
                                None => {
                                    warn!(store_path=%store_path, "no drv found");
                                    // let StdIO take over
                                    return Ok(None);
                                }
                            }
                        };
                        let span = Span::current();
                        span.pb_start();
                        span.pb_set_style(&tvix_tracing::PB_SPINNER_STYLE);
                        span.pb_set_message(&format!("⏳Waiting for inputs {}", &store_path));

                        // derivation_to_build_request needs castore nodes for all inputs.
                        // Provide them, which means, here is where we recursively build
                        // all dependencies.
                        let mut inputs: BTreeMap<StorePath<String>, Node> =
                            futures::stream::iter(drv.input_derivations.iter())
                                .map(|(input_drv_path, output_names)| {
                                    // look up the derivation object
                                    let input_drv = {
                                        let known_paths = self.known_paths.borrow();
                                        known_paths
                                            .get_drv_by_drvpath(input_drv_path)
                                            .unwrap_or_else(|| {
                                                panic!("{} not found", input_drv_path)
                                            })
                                            .to_owned()
                                    };

                                    // convert output names to actual paths
                                    let output_paths: Vec<StorePath<String>> = output_names
                                        .iter()
                                        .map(|output_name| {
                                            input_drv
                                                .outputs
                                                .get(output_name)
                                                .expect("missing output_name")
                                                .path
                                                .as_ref()
                                                .expect("missing output path")
                                                .clone()
                                        })
                                        .collect();

                                    // For each output, ask for the castore node.
                                    // We're in a per-derivation context, so if they're
                                    // not built yet they'll all get built together.
                                    // If they don't need to build, we can however still
                                    // substitute all in parallel (if they don't need to
                                    // be built) - so we turn this into a stream of streams.
                                    // It's up to the builder to deduplicate same build requests.
                                    futures::stream::iter(output_paths.into_iter()).map(
                                        |output_path| async move {
                                            let node = self
                                                .store_path_to_node(&output_path, Path::new(""))
                                                .await?;

                                            if let Some(node) = node {
                                                Ok((output_path, node))
                                            } else {
                                                Err(io::Error::other("no node produced"))
                                            }
                                        },
                                    )
                                })
                                .flatten()
                                .buffer_unordered(
                                    1, /* TODO: increase again once we prevent redundant fetches */
                                ) // TODO: make configurable
                                .try_collect()
                                .await?;

                        // FUTUREWORK: merge these who things together
                        // add input sources
                        let input_sources: BTreeMap<_, _> =
                            futures::stream::iter(drv.input_sources.iter())
                                .then(|input_source| {
                                    Box::pin({
                                        let input_source = input_source.clone();
                                        async move {
                                            let node = self
                                                .store_path_to_node(&input_source, Path::new(""))
                                                .await?;
                                            if let Some(node) = node {
                                                Ok((input_source, node))
                                            } else {
                                                Err(io::Error::other("no node produced"))
                                            }
                                        }
                                    })
                                })
                                .try_collect()
                                .await?;

                        inputs.extend(input_sources);

                        span.pb_set_message(&format!("🔨Building {}", &store_path));

                        // TODO: check if input sources are sufficiently dealth with,
                        // I think yes, they must be imported into the store by other
                        // operations, so dealt with in the Some(…) match arm

                        // synthesize the build request.
                        let build_request = derivation_to_build_request(&drv, inputs)?;

                        // create a build
                        let build_result = self
                            .build_service
                            .as_ref()
                            .do_build(build_request)
                            .await
                            .map_err(|e| std::io::Error::new(io::ErrorKind::Other, e))?;

                        // Maps from the index in refscan_needles to the full store path
                        // Used to map back to the actual store path from the found needles
                        // Importantly, this must match the order of the needles generated in derivation_to_build_request
                        let refscan_needles =
                            crate::tvix_build::get_refscan_needles(&drv).collect::<Vec<_>>();

                        // For each output, insert a PathInfo.
                        for ((output, output_needles), drv_output) in build_result
                            .outputs
                            .iter()
                            .zip(build_result.outputs_needles.iter())
                            .zip(drv.outputs.iter())
                        {
                            let output_node = output
                                .clone()
                                .try_into_anonymous_node()
                                .expect("invalid node");

                            let output_needles: Vec<_> = output_needles
                                .needles
                                .iter()
                                // Map each output needle index back to the refscan_needle
                                .map(|idx| {
                                    refscan_needles
                                        .get(*idx as usize)
                                        .ok_or(std::io::Error::new(
                                            std::io::ErrorKind::Other,
                                            "invalid build response",
                                        ))
                                })
                                .collect::<Result<_, std::io::Error>>()?;

                            // calculate the nar representation
                            let (nar_size, nar_sha256) = self
                                .nar_calculation_service
                                .calculate_nar(&output_node)
                                .await?;

                            // assemble the PathInfo to persist
                            let path_info = PathInfo {
                                store_path: drv_output
                                    .1
                                    .path
                                    .as_ref()
                                    .ok_or(std::io::Error::new(
                                        std::io::ErrorKind::Other,
                                        "Tvix bug: missing output store path",
                                    ))?
                                    .to_owned(),
                                node: output_node,
                                references: output_needles
                                    .iter()
                                    .map(|s| (**s).to_owned())
                                    .collect(),
                                nar_size,
                                nar_sha256,
                                signatures: vec![],
                                deriver: Some(
                                    StorePath::from_name_and_digest_fixed(
                                        drv_path
                                            .name()
                                            .strip_suffix(".drv")
                                            .expect("missing .drv suffix"),
                                        *drv_path.digest(),
                                    )
                                    .expect(
                                        "Tvix bug: StorePath without .drv suffix must be valid",
                                    ),
                                ),
                                ca: drv.fod_digest().map(|fod_digest| {
                                    CAHash::Nar(nix_compat::nixhash::NixHash::Sha256(fod_digest))
                                }),
                            };

                            self.path_info_service
                                .put(path_info)
                                .await
                                .map_err(|e| std::io::Error::new(io::ErrorKind::Other, e))?;
                        }

                        // find the output for the store path requested
                        let s = store_path.to_string();

                        build_result
                            .outputs
                            .into_iter()
                            .map(|e| e.try_into_name_and_node().expect("invalid node"))
                            .find(|(output_name, _output_node)| {
                                output_name.as_ref() == s.as_bytes()
                            })
                            .expect("build didn't produce the store path")
                            .1
                    }
                }
            }
        };

        // now with the root_node and sub_path, descend to the node requested.
        // We convert sub_path to the castore model here.
        let sub_path = tvix_castore::PathBuf::from_host_path(sub_path, true)?;

        directoryservice::descend_to(&self.directory_service, root_node, sub_path)
            .await
            .map_err(|e| std::io::Error::new(io::ErrorKind::Other, e))
    }
}

impl EvalIO for TvixStoreIO {
    #[instrument(skip(self), ret(level = Level::TRACE), err)]
    fn path_exists(&self, path: &Path) -> io::Result<bool> {
        if let Ok((store_path, sub_path)) = StorePath::from_absolute_path_full(path) {
            if self
                .tokio_handle
                .block_on(self.store_path_to_node(&store_path, sub_path))?
                .is_some()
            {
                Ok(true)
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                self.std_io.path_exists(path)
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.path_exists(path)
        }
    }

    #[instrument(skip(self), err)]
    fn open(&self, path: &Path) -> io::Result<Box<dyn io::Read>> {
        if let Ok((store_path, sub_path)) = StorePath::from_absolute_path_full(path) {
            if let Some(node) = self
                .tokio_handle
                .block_on(async { self.store_path_to_node(&store_path, sub_path).await })?
            {
                // depending on the node type, treat open differently
                match node {
                    Node::Directory { .. } => {
                        // This would normally be a io::ErrorKind::IsADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            format!("tried to open directory at {:?}", path),
                        ))
                    }
                    Node::File { digest, .. } => {
                        self.tokio_handle.block_on(async {
                            let resp = self.blob_service.as_ref().open_read(&digest).await?;
                            match resp {
                                Some(blob_reader) => {
                                    // The VM Response needs a sync [std::io::Reader].
                                    Ok(Box::new(SyncIoBridge::new(blob_reader))
                                        as Box<dyn io::Read>)
                                }
                                None => {
                                    error!(
                                        blob.digest = %digest,
                                        "blob not found",
                                    );
                                    Err(io::Error::new(
                                        io::ErrorKind::NotFound,
                                        format!("blob {} not found", &digest),
                                    ))
                                }
                            }
                        })
                    }
                    Node::Symlink { .. } => Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "open for symlinks is unsupported",
                    ))?,
                }
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                self.std_io.open(path)
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.open(path)
        }
    }

    #[instrument(skip(self), ret(level = Level::TRACE), err)]
    fn file_type(&self, path: &Path) -> io::Result<FileType> {
        if let Ok((store_path, sub_path)) = StorePath::from_absolute_path_full(path) {
            if let Some(node) = self
                .tokio_handle
                .block_on(async { self.store_path_to_node(&store_path, sub_path).await })?
            {
                match node {
                    Node::Directory { .. } => Ok(FileType::Directory),
                    Node::File { .. } => Ok(FileType::Regular),
                    Node::Symlink { .. } => Ok(FileType::Symlink),
                }
            } else {
                self.std_io.file_type(path)
            }
        } else {
            self.std_io.file_type(path)
        }
    }

    #[instrument(skip(self), ret(level = Level::TRACE), err)]
    fn read_dir(&self, path: &Path) -> io::Result<Vec<(bytes::Bytes, FileType)>> {
        if let Ok((store_path, sub_path)) = StorePath::from_absolute_path_full(path) {
            if let Some(node) = self
                .tokio_handle
                .block_on(async { self.store_path_to_node(&store_path, sub_path).await })?
            {
                match node {
                    Node::Directory { digest, .. } => {
                        // fetch the Directory itself.
                        if let Some(directory) = self.tokio_handle.block_on({
                            let digest = digest.clone();
                            async move { self.directory_service.as_ref().get(&digest).await }
                        })? {
                            let mut children: Vec<(bytes::Bytes, FileType)> = Vec::new();
                            for (name, node) in directory.into_nodes() {
                                children.push(match node {
                                    Node::Directory { .. } => (name.into(), FileType::Directory),
                                    Node::File { .. } => (name.clone().into(), FileType::Regular),
                                    Node::Symlink { .. } => (name.into(), FileType::Symlink),
                                })
                            }
                            Ok(children)
                        } else {
                            // If we didn't get the directory node that's linked, that's a store inconsistency!
                            error!(
                                directory.digest = %digest,
                                path = ?path,
                                "directory not found",
                            );
                            Err(io::Error::new(
                                io::ErrorKind::NotFound,
                                format!("directory {digest} does not exist"),
                            ))?
                        }
                    }
                    Node::File { .. } => {
                        // This would normally be a io::ErrorKind::NotADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            "tried to readdir path {:?}, which is a file",
                        ))?
                    }
                    Node::Symlink { .. } => Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "read_dir for symlinks is unsupported",
                    ))?,
                }
            } else {
                self.std_io.read_dir(path)
            }
        } else {
            self.std_io.read_dir(path)
        }
    }

    #[instrument(skip(self), ret(level = Level::TRACE), err)]
    fn import_path(&self, path: &Path) -> io::Result<PathBuf> {
        let path_info = self.tokio_handle.block_on({
            tvix_store::import::import_path_as_nar_ca(
                path,
                tvix_store::import::path_to_name(path)?,
                &self.blob_service,
                &self.directory_service,
                &self.path_info_service,
                &self.nar_calculation_service,
            )
        })?;

        // From the returned PathInfo, extract the store path and return it.
        Ok(path_info.store_path.to_absolute_path().into())
    }

    #[instrument(skip(self), ret(level = Level::TRACE))]
    fn store_dir(&self) -> Option<String> {
        Some("/nix/store".to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::{path::Path, rc::Rc, sync::Arc};

    use bstr::ByteSlice;
    use clap::Parser;
    use tempfile::TempDir;
    use tvix_build::buildservice::DummyBuildService;
    use tvix_eval::{EvalIO, EvaluationResult};
    use tvix_store::utils::{construct_services, ServiceUrlsMemory};

    use super::TvixStoreIO;
    use crate::builtins::{add_derivation_builtins, add_fetcher_builtins, add_import_builtins};

    /// evaluates a given nix expression and returns the result.
    /// Takes care of setting up the evaluator so it knows about the
    // `derivation` builtin.
    fn eval(str: &str) -> EvaluationResult {
        let tokio_runtime = tokio::runtime::Runtime::new().unwrap();
        let (blob_service, directory_service, path_info_service, nar_calculation_service) =
            tokio_runtime
                .block_on(async {
                    construct_services(ServiceUrlsMemory::parse_from(std::iter::empty::<&str>()))
                        .await
                })
                .unwrap();

        let io = Rc::new(TvixStoreIO::new(
            blob_service,
            directory_service,
            path_info_service,
            nar_calculation_service.into(),
            Arc::<DummyBuildService>::default(),
            tokio_runtime.handle().clone(),
        ));

        let mut eval_builder =
            tvix_eval::Evaluation::builder(io.clone() as Rc<dyn EvalIO>).enable_import();
        eval_builder = add_derivation_builtins(eval_builder, Rc::clone(&io));
        eval_builder = add_fetcher_builtins(eval_builder, Rc::clone(&io));
        eval_builder = add_import_builtins(eval_builder, io);
        let eval = eval_builder.build();

        // run the evaluation itself.
        eval.evaluate(str, None)
    }

    /// Helper function that takes a &Path, and invokes a tvix evaluator coercing that path to a string
    /// (via "${/this/path}"). The path can be both absolute or not.
    /// It returns Option<String>, depending on whether the evaluation succeeded or not.
    fn import_path_and_compare<P: AsRef<Path>>(p: P) -> Option<String> {
        // Try to import the path using "${/tmp/path/to/test}".
        // The format string looks funny, the {} passed to Nix needs to be
        // escaped.
        let code = format!(r#""${{{}}}""#, p.as_ref().display());
        let result = eval(&code);

        if !result.errors.is_empty() {
            return None;
        }

        let value = result.value.expect("must be some");
        match value {
            tvix_eval::Value::String(s) => Some(s.to_str_lossy().into_owned()),
            _ => panic!("unexpected value type: {:?}", value),
        }
    }

    /// Import a directory with a zero-sized ".keep" regular file.
    /// Ensure it matches the (pre-recorded) store path that Nix would produce.
    #[test]
    fn import_directory() {
        let tmpdir = TempDir::new().unwrap();

        // create a directory named "test"
        let src_path = tmpdir.path().join("test");
        std::fs::create_dir(&src_path).unwrap();

        // write a regular file `.keep`.
        std::fs::write(src_path.join(".keep"), vec![]).unwrap();

        // importing the path with .../test at the end.
        assert_eq!(
            Some("/nix/store/gq3xcv4xrj4yr64dflyr38acbibv3rm9-test".to_string()),
            import_path_and_compare(&src_path)
        );

        // importing the path with .../test/. at the end.
        assert_eq!(
            Some("/nix/store/gq3xcv4xrj4yr64dflyr38acbibv3rm9-test".to_string()),
            import_path_and_compare(src_path.join("."))
        );
    }

    /// Import a file into the store. Nix uses the "recursive"/NAR-based hashing
    /// scheme for these.
    #[test]
    fn import_file() {
        let tmpdir = TempDir::new().unwrap();

        // write a regular file `empty`.
        std::fs::write(tmpdir.path().join("empty"), vec![]).unwrap();

        assert_eq!(
            Some("/nix/store/lx5i78a4izwk2qj1nq8rdc07y8zrwy90-empty".to_string()),
            import_path_and_compare(tmpdir.path().join("empty"))
        );

        // write a regular file `hello.txt`.
        std::fs::write(tmpdir.path().join("hello.txt"), b"Hello World!").unwrap();

        assert_eq!(
            Some("/nix/store/925f1jb1ajrypjbyq7rylwryqwizvhp0-hello.txt".to_string()),
            import_path_and_compare(tmpdir.path().join("hello.txt"))
        );
    }

    /// Invoke toString on a nonexisting file, and access the .file attribute.
    /// This should not cause an error, because it shouldn't trigger an import,
    /// and leave the path as-is.
    #[test]
    fn nonexisting_path_without_import() {
        let result = eval("toString ({ line = 42; col = 42; file = /deep/thought; }.file)");

        assert!(result.errors.is_empty(), "expect evaluation to succeed");
        let value = result.value.expect("must be some");

        match value {
            tvix_eval::Value::String(s) => {
                assert_eq!(*s, "/deep/thought");
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }
}
