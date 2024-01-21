use std::path::PathBuf;

use lazy_static::lazy_static;
use nix_compat::{derivation::Derivation, store_path::StorePath};
use tracing::trace;

lazy_static! {
    static ref TMPDIR: PathBuf = {
        std::fs::create_dir_all("/tmp/tvix").expect("failed to create tempdir parent");
        let dir = tempfile::tempdir_in("/tmp/tvix")
            .expect("failed to create tempdir")
            .into_path();
        println!("tvix tempdir: {}", dir.to_string_lossy());
        dir
    };
}

/// Dumps a derivation in a store-like structure consisting only of `.drv`, such a tree of files
/// can be used in conjunction with a Tvix tool to determine the root cause of a divergence (i.e. a
/// difference between Nix and Tvix).
pub(crate) fn dump_drv_to_store(drv_path: &StorePath, drv: &Derivation) {
    use std::io::Write;
    use std::path::Path;

    let mut file_path = TMPDIR.clone();
    file_path.push(format!("{}", drv_path));

    // A Derivation might produce .drv files with the same env name, but different hashes.
    // If the same thing already existed, remove it before adding it again
    if Path::new(&file_path).exists() {
        std::fs::remove_file(&file_path).expect("failed to remove previous drv");
    }

    let mut file = std::fs::File::create(&file_path).expect("failed to create tmpfile");
    file.write_all(&drv.to_aterm_bytes())
        .expect("failed to write drv to tmpfile");

    trace!("dumped {:?} into {}", drv_path, file_path.display());
}
