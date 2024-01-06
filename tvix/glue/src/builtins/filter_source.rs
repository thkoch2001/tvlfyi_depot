// Example of how `builtins.filterSource` can be done by having a
// store interface that does not rely on closures.

use std::path::PathBuf;

use tvix_eval::builtin_macros::builtins;
use tvix_eval::generators::{self, Gen, GenCo};
use tvix_eval::*;

// Whatever representation of a candidate there is, it should have all
// the details that eval needs to make decisions (in filterSource/path
// that's, I believe, the name and the type of the thing).
struct Candidate {
    file_type: &'static str,
    name: String,
}

// InsertionHandle (or whatever it should be called, didn't think much
// about it) produces a list of candidates and then lets the caller
// decide whether to include the candidate or not.
struct InsertionHandle {
    // in the real-world this iterator lives in the store side of
    // things, or wherever, it shouldn't matter to the usage in eval,
    // it's just an example here.
    iter: walkdir::IntoIter,

    // current entry, tracked to deal with the acceptance case
    current: Option<walkdir::DirEntry>,
}

// The store does this ...
fn new_handle(p: PathBuf) -> InsertionHandle {
    // Skip the first entry, as it yields the directory to be scanned
    // *itself*, which we don't want.
    let mut iter = walkdir::WalkDir::new(p).into_iter();
    iter.next();

    InsertionHandle {
        iter,
        current: None,
    }
}

impl InsertionHandle {
    // Eval repeatedly calls this until it's None.
    fn next_candidate(&mut self) -> Option<Candidate> {
        // If the current entry was skipped, and is a directory, we
        // skip that whole subtree.
        if let Some(c) = &self.current {
            if c.file_type().is_dir() {
                self.iter.skip_current_dir();
            }
        }

        let next = self.iter.next().map(|e| e.expect("errors are for later"));
        self.current = next;

        let entry = self.current.as_ref()?;
        let file_type = if entry.file_type().is_dir() {
            "directory"
        } else if entry.file_type().is_file() {
            "regular"
        } else if entry.file_type().is_symlink() {
            "symlink"
        } else {
            "unknown"
        };

        Some(Candidate {
            file_type,
            name: self.current.as_ref()?.path().to_string_lossy().to_string(),
        })
    }

    // Eval calls this only when the last candidate passed the filter.
    fn accept(&mut self) {
        if let Some(c) = self.current.take() {
            println!("accepted file: {}", c.path().to_string_lossy());
        }
    }
}

// Then on the eval side things are fine & dandy:

#[builtins]
pub(crate) mod source_builtins {
    use super::*;

    #[builtin("filterSource")]
    async fn builtin_filter_source(
        co: GenCo,
        #[lazy] filter: Value,
        path: Value,
    ) -> Result<Value, ErrorKind> {
        let p = path.to_path()?;

        // in reality, get this from the store
        let mut handle = new_handle(*p);

        while let Some(candidate) = handle.next_candidate() {
            let result = generators::request_force(
                &co,
                generators::request_call_with(
                    &co,
                    filter.clone(),
                    [
                        Value::String(candidate.name.into()),
                        Value::String(candidate.file_type.into()),
                    ],
                )
                .await,
            )
            .await;

            if result.as_bool()? {
                handle.accept();
            }
        }

        Ok(Value::Bool(true)) // return the store path or whatever
    }
}

pub use source_builtins::builtins as source_builtins;
