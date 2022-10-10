use std::convert::Infallible;
use std::io;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use crate::errors::ErrorKind;

#[derive(Debug, Clone, PartialEq, Eq)]
enum NixPathEntry {
    Path(PathBuf),
    Prefix { prefix: PathBuf, path: PathBuf },
}

impl NixPathEntry {
    fn resolve(&self, lookup_path: &Path) -> io::Result<Option<PathBuf>> {
        let resolve_in =
            |parent: &Path, lookup_path: &Path| match parent.join(lookup_path).canonicalize() {
                Ok(path) => Ok(Some(path)),
                Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(None),
                Err(e) => Err(e),
            };

        match self {
            NixPathEntry::Path(p) => resolve_in(p, lookup_path),
            NixPathEntry::Prefix { prefix, path } => {
                if let Ok(child_path) = dbg!(dbg!(lookup_path).strip_prefix(dbg!(prefix))) {
                    resolve_in(path, child_path)
                } else {
                    Ok(None)
                }
            }
        }
    }
}

impl FromStr for NixPathEntry {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once('=') {
            Some((prefix, path)) => Ok(Self::Prefix {
                prefix: prefix.into(),
                path: path.into(),
            }),
            None => Ok(Self::Path(s.into())),
        }
    }
}

/// Struct implementing the format and path resolution rules of the `NIX_PATH`
/// environment variable.
///
/// This struct can be constructed by parsing a string using the [`FromStr`]
/// impl, or via [`str::parse`]. Nix `<...>` paths can then be resolved using
/// [`NixPath::resolve`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NixPath {
    entries: Vec<NixPathEntry>,
}

impl NixPath {
    /// Attempt to resolve the given `path` within this [`NixPath`] using the
    /// path resolution rules for `<...>`-style paths
    #[allow(dead_code)] // TODO(grfn)
    pub fn resolve<P>(&self, path: P) -> Result<PathBuf, ErrorKind>
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref();
        for entry in &self.entries {
            if let Some(p) = entry.resolve(path)? {
                return Ok(p);
            }
        }
        Err(ErrorKind::PathResolution(format!(
            "path '{}' was not found in the Nix search path",
            path.display()
        )))
    }
}

impl FromStr for NixPath {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let entries = s
            .split(':')
            .map(|s| s.parse())
            .collect::<Result<Vec<_>, _>>()?;
        Ok(NixPath { entries })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;

        // Examples from https://nixos.org/manual/nix/stable/command-ref/env-common.html#env-NIX_PATH

        #[test]
        fn doc_example_1() {
            assert_eq!(
                NixPath::from_str("/home/eelco/Dev:/etc/nixos").unwrap(),
                NixPath {
                    entries: vec![
                        NixPathEntry::Path("/home/eelco/Dev".into()),
                        NixPathEntry::Path("/etc/nixos".into())
                    ],
                }
            );
        }

        #[test]
        fn doc_example_2() {
            assert_eq!(
                NixPath::from_str("nixpkgs=/home/eelco/Dev/nixpkgs-branch:/etc/nixos").unwrap(),
                NixPath {
                    entries: vec![
                        NixPathEntry::Prefix {
                            prefix: "nixpkgs".into(),
                            path: "/home/eelco/Dev/nixpkgs-branch".into()
                        },
                        NixPathEntry::Path("/etc/nixos".into())
                    ],
                }
            );
        }
    }

    mod resolve {
        use std::env::current_dir;

        use path_clean::PathClean;

        use super::*;

        #[test]
        fn simple_dir() {
            let nix_path = NixPath::from_str("./.").unwrap();
            let res = nix_path.resolve("src").unwrap();
            assert_eq!(res, current_dir().unwrap().join("src").clean());
        }

        #[test]
        fn failed_resolution() {
            let nix_path = NixPath::from_str("./.").unwrap();
            let err = nix_path.resolve("nope").unwrap_err();
            assert!(
                matches!(err, ErrorKind::PathResolution(..)),
                "err = {err:?}"
            );
        }

        #[test]
        fn second_in_path() {
            let nix_path = NixPath::from_str("./.:/").unwrap();
            let res = nix_path.resolve("bin").unwrap();
            assert_eq!(res, Path::new("/bin"));
        }

        #[test]
        fn prefix() {
            let nix_path = NixPath::from_str("/:tvix=.").unwrap();
            let res = nix_path.resolve("tvix/src").unwrap();
            assert_eq!(res, current_dir().unwrap().join("src").clean());
        }
    }
}
