//! This module implements a wrapper around tvix-eval's [EvalIO] type
//! which handles tvix-cli specific operations, such as marking plain
//! paths as known to the reference scanner.
//!
//! All uses of [EvalIO] in tvix-cli should make use of this wrapper.

use crate::KnownPaths;
use smol_str::SmolStr;
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use tvix_eval::{ErrorKind, EvalIO, FileType};

pub(crate) struct TvixIO<T: EvalIO> {
    /// Ingested paths must be reported to this known paths tracker
    /// for accurate build reference scanning.
    known_paths: Rc<RefCell<KnownPaths>>,

    // Actual underlying [EvalIO] implementation.
    actual: T,
}

impl<T: EvalIO> TvixIO<T> {
    pub(crate) fn new(known_paths: Rc<RefCell<KnownPaths>>, actual: T) -> Self {
        Self {
            known_paths,
            actual,
        }
    }
}

impl<T: EvalIO> EvalIO for TvixIO<T> {
    fn store_dir(&self) -> Option<String> {
        self.actual.store_dir()
    }

    fn import_path(&mut self, path: &Path) -> Result<PathBuf, ErrorKind> {
        let imported_path = self.actual.import_path(path)?;
        self.known_paths
            .borrow_mut()
            .plain(imported_path.to_string_lossy());

        Ok(imported_path)
    }

    fn path_exists(&mut self, path: PathBuf) -> Result<bool, ErrorKind> {
        if path.starts_with("/__corepkgs__") {
            return Ok(true);
        }

        self.actual.path_exists(path)
    }

    fn read_to_string(&mut self, path: PathBuf) -> Result<String, ErrorKind> {
        // Bundled version of corepkgs/fetchurl.nix. This workaround
        // is similar to what cppnix does for passing the path
        // through.
        //
        // TODO: this comparison is bad and allocates, we should use
        // the sane path library.
        if path.starts_with("/__corepkgs__/fetchurl.nix") {
            return Ok(include_str!("fetchurl.nix").to_string());
        }

        self.actual.read_to_string(path)
    }

    fn read_dir(&mut self, path: PathBuf) -> Result<Vec<(SmolStr, FileType)>, ErrorKind> {
        self.actual.read_dir(path)
    }
}
