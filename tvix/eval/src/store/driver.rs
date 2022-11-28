use super::derivation::Derivation;

/// A "store driver" for Tvix is like a "database driver" in the SQL
/// database world.  It's a small piece of code that runs in the
/// client process; it knows how to speak the store's wire protocol
/// and may do some minimal amount of client-side caching/pooling to
/// hide latency.  Different store implementations will typically
/// require different store drivers.
pub trait StoreDriver {
    // The default implementations of `getDrvName()` and
    // `getOutPath` do their calculation locally, in the evaluator
    // process.  Out-of-process stores can reimplement `getDrvName`
    // and `getOutPath` in order to spawn a thread/task to send the
    // Derivation to the store concurrently with the rest of
    // evaluation.  Derivations are built by walking the dependency
    // tree leaf-to-root, and due to lazy evaluation of nixlang
    // there will be considerable computation interleaved with these
    // calls at the lower levels of the tree.

    /// Calculate the filename of a derivation.
    fn get_drv_path(&self, derivation: &Derivation) -> String {
        let mut res = "".to_string();
        // TODO(amjoseph): calculate correct hash
        res.push_str("00000000000000000000000000000000-");
        res.push_str(&derivation.name);
        res.push_str(".drv");
        res.to_owned()
    }

    /// Calculate the outpath for a given output.
    fn get_out_path(&self, derivation: &Derivation, output: &str) -> String {
        let mut res = "".to_string();
        // TODO(amjoseph): calculate correct hash
        res.push_str("00000000000000000000000000000000-");
        res.push_str(output);
        res.push_str("-");
        res.push_str(&derivation.name);
        res
    }
}

struct DefaultStoreDriver;

impl StoreDriver for DefaultStoreDriver {}

pub fn default_store_driver() -> impl StoreDriver {
    DefaultStoreDriver
}
