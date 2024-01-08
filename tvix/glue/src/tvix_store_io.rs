//! This module provides an implementation of EvalIO talking to tvix-store.

use nix_compat::store_path::StorePath;
use std::{
    cell::RefCell,
    io,
    path::{Path, PathBuf},
};
use tokio::io::AsyncReadExt;
use tracing::{error, instrument, warn};
use tvix_eval::{EvalIO, FileType, StdIO};

use tvix_castore::{
    blobservice::BlobService,
    directoryservice::{self, DirectoryService},
    proto::node::Node,
    B3Digest,
};
use tvix_store::pathinfoservice::PathInfoService;

use crate::known_paths::KnownPaths;

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
pub struct TvixStoreIO<BS, DS, PS> {
    blob_service: BS,
    directory_service: DS,
    path_info_service: PS,
    std_io: StdIO,
    tokio_handle: tokio::runtime::Handle,
    pub(crate) known_paths: RefCell<KnownPaths>,
}

impl<BS, DS, PS> TvixStoreIO<BS, DS, PS>
where
    DS: AsRef<dyn DirectoryService>,
    PS: AsRef<dyn PathInfoService>,
{
    pub fn new(
        blob_service: BS,
        directory_service: DS,
        path_info_service: PS,
        tokio_handle: tokio::runtime::Handle,
    ) -> Self {
        Self {
            blob_service,
            directory_service,
            path_info_service,
            std_io: StdIO {},
            tokio_handle,
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
    #[instrument(skip(self), ret, err)]
    async fn store_path_to_node(
        &self,
        store_path: &StorePath,
        sub_path: &Path,
    ) -> io::Result<Option<Node>> {
        let root_node = match self
            .path_info_service
            .as_ref()
            .get(*store_path.digest())
            .await?
        {
            // if we have a PathInfo, we know there will be a root_node (due to validation)
            Some(path_info) => path_info.node.expect("no node").node.expect("no node"),
            // If there's no PathInfo found, we didn't build that path yet.
            // and have to trigger the build (and probably insert into the
            // PathInfoService (which requires refscan))
            // FUTUREWORK: We don't do builds yet, so log a warning and let
            // std_io take over.
            // In the future, not getting a root node means a failed build!
            None => {
                warn!("would trigger build, skipping");
                return Ok(None);
            }
        };

        // with the root_node and sub_path, descend to the node requested.
        directoryservice::descend_to(&self.directory_service, root_node, sub_path)
            .await
            .map_err(|e| std::io::Error::new(io::ErrorKind::Other, e))
    }

    fn store_path_to_node_sync(
        &self,
        store_path: &StorePath,
        sub_path: &Path,
    ) -> io::Result<Option<Node>> {
        self.tokio_handle
            .block_on(async { self.store_path_to_node(store_path, sub_path).await })
    }
}

impl<BS, DS, PS> EvalIO for TvixStoreIO<BS, DS, PS>
where
    BS: AsRef<dyn BlobService> + Clone,
    DS: AsRef<dyn DirectoryService>,
    PS: AsRef<dyn PathInfoService>,
{
    #[instrument(skip(self), ret, err)]
    fn path_exists(&self, path: &Path) -> io::Result<bool> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if self
                .store_path_to_node_sync(&store_path, &sub_path)?
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

    #[instrument(skip(self), ret, err)]
    fn read_to_string(&self, path: &Path) -> io::Result<String> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self.store_path_to_node_sync(&store_path, &sub_path)? {
                // depending on the node type, treat read_to_string differently
                match node {
                    Node::Directory(_) => {
                        // This would normally be a io::ErrorKind::IsADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            format!("tried to read directory at {:?} to string", path),
                        ))
                    }
                    Node::File(file_node) => {
                        let digest: B3Digest =
                            file_node.digest.clone().try_into().map_err(|_e| {
                                error!(
                                    file_node = ?file_node,
                                    "invalid digest"
                                );
                                io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    format!("invalid digest length in file node: {:?}", file_node),
                                )
                            })?;

                        self.tokio_handle.block_on(async {
                            let mut reader = {
                                let resp = self.blob_service.as_ref().open_read(&digest).await?;
                                match resp {
                                    Some(blob_reader) => blob_reader,
                                    None => {
                                        error!(
                                            blob.digest = %digest,
                                            "blob not found",
                                        );
                                        Err(io::Error::new(
                                            io::ErrorKind::NotFound,
                                            format!("blob {} not found", &digest),
                                        ))?
                                    }
                                }
                            };

                            let mut buf = String::new();

                            reader.read_to_string(&mut buf).await?;
                            Ok(buf)
                        })
                    }
                    Node::Symlink(_symlink_node) => Err(io::Error::new(
                        io::ErrorKind::Unsupported,
                        "read_to_string for symlinks is unsupported",
                    ))?,
                }
            } else {
                // As tvix-store doesn't manage /nix/store on the filesystem,
                // we still need to also ask self.std_io here.
                self.std_io.read_to_string(path)
            }
        } else {
            // The store path is no store path, so do regular StdIO.
            self.std_io.read_to_string(path)
        }
    }

    #[instrument(skip(self), ret, err)]
    fn read_dir(&self, path: &Path) -> io::Result<Vec<(bytes::Bytes, FileType)>> {
        if let Ok((store_path, sub_path)) =
            StorePath::from_absolute_path_full(&path.to_string_lossy())
        {
            if let Some(node) = self.store_path_to_node_sync(&store_path, &sub_path)? {
                match node {
                    Node::Directory(directory_node) => {
                        // fetch the Directory itself.
                        let digest: B3Digest =
                            directory_node.digest.clone().try_into().map_err(|_e| {
                                io::Error::new(
                                    io::ErrorKind::InvalidData,
                                    format!(
                                        "invalid digest length in directory node: {:?}",
                                        directory_node
                                    ),
                                )
                            })?;

                        if let Some(directory) = self.tokio_handle.block_on(async {
                            self.directory_service.as_ref().get(&digest).await
                        })? {
                            let mut children: Vec<(bytes::Bytes, FileType)> = Vec::new();
                            for node in directory.nodes() {
                                children.push(match node {
                                    Node::Directory(e) => (e.name, FileType::Directory),
                                    Node::File(e) => (e.name, FileType::Regular),
                                    Node::Symlink(e) => (e.name, FileType::Symlink),
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
                    Node::File(_file_node) => {
                        // This would normally be a io::ErrorKind::NotADirectory (still unstable)
                        Err(io::Error::new(
                            io::ErrorKind::Unsupported,
                            "tried to readdir path {:?}, which is a file",
                        ))?
                    }
                    Node::Symlink(_symlink_node) => Err(io::Error::new(
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

    #[instrument(skip(self), ret, err)]
    fn import_path(&self, path: &Path) -> io::Result<PathBuf> {
        let output_path = self.tokio_handle.block_on(async {
            tvix_store::import::import_path(
                path,
                &self.blob_service,
                &self.directory_service,
                &self.path_info_service,
            )
            .await
        })?;

        Ok(output_path.to_absolute_path().into())
    }

    #[instrument(skip(self), ret)]
    fn store_dir(&self) -> Option<String> {
        Some("/nix/store".to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::{path::Path, sync::Arc};

    use tempfile::TempDir;
    use tvix_castore::{
        blobservice::{BlobService, MemoryBlobService},
        directoryservice::{DirectoryService, MemoryDirectoryService},
    };
    use tvix_eval::EvaluationResult;
    use tvix_store::pathinfoservice::{MemoryPathInfoService, PathInfoService};

    use crate::builtins::add_derivation_builtins;

    use super::TvixStoreIO;

    /// evaluates a given nix expression and returns the result.
    /// Takes care of setting up the evaluator so it knows about the
    // `derivation` builtin.
    fn eval(str: &str) -> EvaluationResult {
        let mut eval = tvix_eval::Evaluation::new_impure();

        let blob_service = Arc::new(MemoryBlobService::default()) as Arc<dyn BlobService>;
        let directory_service =
            Arc::new(MemoryDirectoryService::default()) as Arc<dyn DirectoryService>;
        let path_info_service = Arc::new(MemoryPathInfoService::new(
            blob_service.clone(),
            directory_service.clone(),
        )) as Arc<dyn PathInfoService>;
        let runtime = tokio::runtime::Runtime::new().unwrap();

        let store = TvixStoreIO::new(
            blob_service.clone(),
            directory_service.clone(),
            path_info_service.clone(),
            runtime.handle().clone(),
        );

        eval.io_handle = Box::new(TvixStoreIO::new(
            blob_service,
            directory_service,
            path_info_service,
            runtime.handle().clone(),
        ));

        add_derivation_builtins(&mut eval, store);

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
            tvix_eval::Value::String(s) => return Some(s.as_str().to_owned()),
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
                assert_eq!("/deep/thought", s.as_str());
            }
            _ => panic!("unexpected value type: {:?}", value),
        }
    }
}
