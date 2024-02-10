//! Simple scanner for non-overlapping, known references of Nix store paths in a
//! given string.
//!
//! This is used for determining build references (see
//! //tvix/eval/docs/build-references.md for more details).
//!
//! The scanner itself is using the Wu-Manber string-matching algorithm, using
//! our fork of the `wu-mamber` crate.

use std::collections::BTreeSet;
use tokio::io::{self, AsyncReadExt};
use tvix_castore::{
    blobservice::BlobService,
    directoryservice::DirectoryService,
    proto::{node::Node, Directory},
};
use wu_manber::TwoByteWM;

pub const STORE_PATH_LEN: usize = "/nix/store/00000000000000000000000000000000".len();

/// Represents a "primed" reference scanner with an automaton that knows the set
/// of store paths to scan for.
pub struct ReferenceScanner<P: Ord + AsRef<[u8]>> {
    candidates: Vec<P>,
    searcher: Option<TwoByteWM>,
    matches: Vec<usize>,
}

impl<P: Clone + Ord + AsRef<[u8]>> ReferenceScanner<P> {
    /// Construct a new `ReferenceScanner` that knows how to scan for the given
    /// candidate store paths.
    pub fn new(candidates: Vec<P>) -> Self {
        let searcher = if candidates.is_empty() {
            None
        } else {
            Some(TwoByteWM::new(&candidates))
        };

        ReferenceScanner {
            searcher,
            candidates,
            matches: Default::default(),
        }
    }

    /// Scan the given str for all non-overlapping matches and collect them
    /// in the scanner.
    pub fn scan<S: AsRef<[u8]>>(&mut self, haystack: S) {
        if haystack.as_ref().len() < STORE_PATH_LEN {
            return;
        }

        if let Some(searcher) = &self.searcher {
            for m in searcher.find(haystack) {
                self.matches.push(m.pat_idx);
            }
        }
    }

    /// Scan the given [`Node`] in the content-addressible store for all matches (including symbolic
    /// links targeting the matches!) and collect them in the scanner
    pub async fn scan_node<BS, DS>(
        &mut self,
        blob_service: BS,
        directory_service: DS,
        root_node: Node,
    ) -> io::Result<()>
    where
        BS: AsRef<dyn BlobService> + Send + Sync + Clone + 'static,
        DS: AsRef<dyn DirectoryService> + Send + Sync + Clone + 'static,
    {
        let directory_service = directory_service.as_ref();
        let blob_service = blob_service.as_ref();

        let mut stack = vec![root_node];
        while let Some(node) = stack.pop() {
            match node {
                Node::Directory(dir) => {
                    let Some(Directory {
                        directories,
                        files,
                        symlinks,
                    }) = directory_service
                        .get(
                            &dir.digest
                                .try_into()
                                .expect("Invalid directory digest in node"),
                        )
                        .await?
                    else {
                        continue; // TODO(aspen): Error if dir doesn't exist?
                    };

                    stack.extend(
                        directories
                            .into_iter()
                            .map(Node::Directory)
                            .chain(files.into_iter().map(Node::File))
                            .chain(symlinks.into_iter().map(Node::Symlink)),
                    )
                }
                Node::File(f) => {
                    let Some(mut blob) = blob_service
                        .open_read(&f.digest.try_into().expect("Invalid file digest in node"))
                        .await?
                    else {
                        continue; // TODO(aspen): Error if dir doesn't exist?
                    };

                    // TODO(aspen): with_capacity? maybe BlobReader should have size_hint?
                    let mut contents = vec![];
                    // TODO(aspen): Is streaming possible?
                    blob.read_to_end(&mut contents).await?;
                    self.scan(contents);
                }
                Node::Symlink(sl) => {
                    if let Some(idx) = self.candidates.iter().position(|p| {
                        let p = p.as_ref();
                        // The symbolic link can point to any sub-path of the store path, so only
                        // look at the prefix
                        &sl.target[..p.len()] == p
                    }) {
                        self.matches.push(idx);
                    }
                }
            }
        }

        Ok(())
    }

    /// Finalise the reference scanner and return the resulting matches.
    pub fn finalise(self) -> BTreeSet<P> {
        self.matches
            .into_iter()
            .map(|idx| self.candidates[idx].clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod scan_strings {
        use super::*;

        // The actual derivation of `nixpkgs.hello`.
        const HELLO_DRV: &str = r#"Derive([("out","/nix/store/33l4p0pn0mybmqzaxfkpppyh7vx1c74p-hello-2.12.1","","")],[("/nix/store/6z1jfnqqgyqr221zgbpm30v91yfj3r45-bash-5.1-p16.drv",["out"]),("/nix/store/ap9g09fxbicj836zm88d56dn3ff4clxl-stdenv-linux.drv",["out"]),("/nix/store/pf80kikyxr63wrw56k00i1kw6ba76qik-hello-2.12.1.tar.gz.drv",["out"])],["/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],"x86_64-linux","/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16/bin/bash",["-e","/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],[("buildInputs",""),("builder","/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16/bin/bash"),("cmakeFlags",""),("configureFlags",""),("depsBuildBuild",""),("depsBuildBuildPropagated",""),("depsBuildTarget",""),("depsBuildTargetPropagated",""),("depsHostHost",""),("depsHostHostPropagated",""),("depsTargetTarget",""),("depsTargetTargetPropagated",""),("doCheck","1"),("doInstallCheck",""),("mesonFlags",""),("name","hello-2.12.1"),("nativeBuildInputs",""),("out","/nix/store/33l4p0pn0mybmqzaxfkpppyh7vx1c74p-hello-2.12.1"),("outputs","out"),("patches",""),("pname","hello"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),("src","/nix/store/pa10z4ngm0g83kx9mssrqzz30s84vq7k-hello-2.12.1.tar.gz"),("stdenv","/nix/store/cp65c8nk29qq5cl1wyy5qyw103cwmax7-stdenv-linux"),("strictDeps",""),("system","x86_64-linux"),("version","2.12.1")])"#;

        #[test]
        fn test_no_patterns() {
            let mut scanner: ReferenceScanner<String> = ReferenceScanner::new(vec![]);

            scanner.scan(HELLO_DRV);

            let result = scanner.finalise();

            assert_eq!(result.len(), 0);
        }

        #[test]
        fn test_single_match() {
            let mut scanner = ReferenceScanner::new(vec![
                "/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16".to_string(),
            ]);
            scanner.scan(HELLO_DRV);

            let result = scanner.finalise();

            assert_eq!(result.len(), 1);
            assert!(result.contains("/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16"));
        }

        #[test]
        fn test_multiple_matches() {
            let candidates = vec![
                // these exist in the drv:
                "/nix/store/33l4p0pn0mybmqzaxfkpppyh7vx1c74p-hello-2.12.1".to_string(),
                "/nix/store/pf80kikyxr63wrw56k00i1kw6ba76qik-hello-2.12.1.tar.gz.drv".to_string(),
                "/nix/store/cp65c8nk29qq5cl1wyy5qyw103cwmax7-stdenv-linux".to_string(),
                // this doesn't:
                "/nix/store/fn7zvafq26f0c8b17brs7s95s10ibfzs-emacs-28.2.drv".to_string(),
            ];

            let mut scanner = ReferenceScanner::new(candidates.clone());
            scanner.scan(HELLO_DRV);

            let result = scanner.finalise();
            assert_eq!(result.len(), 3);

            for c in candidates[..3].iter() {
                assert!(result.contains(c));
            }
        }
    }

    mod scan_nodes {
        use std::os::unix;
        use std::sync::Arc;
        use std::{fs, path::Path};

        use tempfile::TempDir;
        use tvix_castore::utils::{gen_blob_service, gen_directory_service};

        use super::*;

        const HELLO: &str = "/nix/store/sbldylj3clbkc0aqvjjzfa6slp4zdvlj-hello-2.12.1";

        #[tokio::test]
        async fn single_reference() {
            let blob_service = Arc::<dyn BlobService>::from(gen_blob_service());
            let directory_service = Arc::<dyn DirectoryService>::from(gen_directory_service());

            let dir = TempDir::new().unwrap();
            fs::write(dir.path().join("file"), format!("referencing: {HELLO}")).unwrap();

            let dir_node = tvix_castore::import::ingest_path(
                blob_service.clone(),
                directory_service.clone(),
                dir.path(),
            )
            .await
            .unwrap();

            let mut scanner = ReferenceScanner::new(vec![HELLO]);
            scanner
                .scan_node(blob_service.clone(), directory_service.clone(), dir_node)
                .await
                .unwrap();
            let res = scanner.finalise();
            assert_eq!(res, BTreeSet::from([HELLO]))
        }

        #[tokio::test]
        async fn symlink_reference() {
            let blob_service = Arc::<dyn BlobService>::from(gen_blob_service());
            let directory_service = Arc::<dyn DirectoryService>::from(gen_directory_service());

            let dir = TempDir::new().unwrap();
            unix::fs::symlink(
                Path::new(HELLO).join("bin").join("hello"),
                dir.path().join("file"),
            )
            .unwrap();

            let dir_node = tvix_castore::import::ingest_path(
                blob_service.clone(),
                directory_service.clone(),
                dir.path(),
            )
            .await
            .unwrap();

            let mut scanner = ReferenceScanner::new(vec![HELLO]);
            scanner
                .scan_node(blob_service.clone(), directory_service.clone(), dir_node)
                .await
                .unwrap();
            let res = scanner.finalise();
            assert_eq!(res, BTreeSet::from([HELLO]))
        }
    }
}
