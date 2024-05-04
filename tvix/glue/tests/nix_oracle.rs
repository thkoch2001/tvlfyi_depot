//! Tests which use upstream nix as an oracle to test evaluation against

use std::{env, path::PathBuf, process::Command, rc::Rc, sync::Arc};

use pretty_assertions::assert_eq;
use tvix_build::buildservice::DummyBuildService;
use tvix_castore::{
    blobservice::{BlobService, MemoryBlobService},
    directoryservice::{DirectoryService, MemoryDirectoryService},
};
use tvix_eval::{builtins::impure_builtins, EvalIO};
use tvix_glue::{
    builtins::{add_derivation_builtins, add_fetcher_builtins, add_import_builtins},
    tvix_io::TvixIO,
    tvix_store_io::TvixStoreIO,
};
use tvix_store::pathinfoservice::{MemoryPathInfoService, PathInfoService};

fn nix_binary_path() -> PathBuf {
    env::var("NIX_INSTANTIATE_BINARY_PATH")
        .unwrap_or_else(|_| "nix-instantiate".to_owned())
        .into()
}

#[derive(Clone, Copy)]
enum Strictness {
    Lazy,
    Strict,
}

fn nix_eval(expr: &str, strictness: Strictness) -> String {
    let store_dir = tempfile::tempdir().unwrap();

    let mut args = match strictness {
        Strictness::Lazy => vec![],
        Strictness::Strict => vec!["--strict"],
    };
    args.extend_from_slice(&["--eval", "-E"]);

    let output = Command::new(nix_binary_path())
        .args(&args[..])
        .arg(format!("({expr})"))
        .env(
            "NIX_REMOTE",
            format!(
                "local?root={}",
                store_dir
                    .path()
                    .canonicalize()
                    .expect("valid path")
                    .display()
            ),
        )
        .output()
        .unwrap();
    if !output.status.success() {
        panic!(
            "nix eval {expr} failed!\n    stdout: {}\n    stderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
    }

    String::from_utf8(output.stdout).unwrap()
}

/// Compare the evaluation of the given nix expression in nix (using the
/// `NIX_INSTANTIATE_BINARY_PATH` env var to resolve the `nix-instantiate` binary) and tvix, and
/// assert that the result is identical
#[track_caller]
fn compare_eval(expr: &str, strictness: Strictness) {
    let nix_result = nix_eval(expr, strictness);

    let blob_service = Arc::new(MemoryBlobService::default()) as Arc<dyn BlobService>;
    let directory_service =
        Arc::new(MemoryDirectoryService::default()) as Arc<dyn DirectoryService>;
    let path_info_service = Box::new(MemoryPathInfoService::new(
        blob_service.clone(),
        directory_service.clone(),
    )) as Box<dyn PathInfoService>;
    let tokio_runtime = tokio::runtime::Runtime::new().unwrap();

    let tvix_store_io = Rc::new(TvixStoreIO::new(
        blob_service,
        directory_service,
        path_info_service.into(),
        Arc::new(DummyBuildService::default()),
        tokio_runtime.handle().clone(),
    ));

    // Wrap with TvixIO, so <nix/fetchurl.nix can be imported.
    let mut eval = tvix_eval::Evaluation::new(
        Box::new(TvixIO::new(tvix_store_io.clone() as Rc<dyn EvalIO>)) as Box<dyn EvalIO>,
        true,
    );
    eval.strict = matches!(strictness, Strictness::Strict);
    eval.io_handle = Box::new(tvix_eval::StdIO);
    eval.builtins.extend(impure_builtins());
    add_derivation_builtins(&mut eval, Rc::clone(&tvix_store_io));
    add_fetcher_builtins(&mut eval, Rc::clone(&tvix_store_io));
    add_import_builtins(&mut eval, tvix_store_io);

    let tvix_result = eval.evaluate(expr, None);
    for error in &tvix_result.errors {
        error.fancy_format_stderr();
    }
    let tvix_result = dbg!(tvix_result.value)
        .expect("tvix evaluation should succeed")
        .to_string();

    assert_eq!(nix_result.trim(), tvix_result);
}

/// Generate a suite of tests which call [`compare_eval`] on expressions, checking that nix and tvix
/// return identical results.
macro_rules! compare_eval_tests {
    ($strictness:expr, {}) => {};
    ($strictness:expr, {$(#[$meta:meta])* $test_name: ident($expr: expr); $($rest:tt)*}) => {
        #[test]
        $(#[$meta])*
        fn $test_name() {
            compare_eval($expr, $strictness);
        }

        compare_eval_tests!($strictness, { $($rest)* });
    }
}

macro_rules! compare_strict_eval_tests {
    ($($tests:tt)*) => {
        compare_eval_tests!(Strictness::Strict, { $($tests)* });
    }
}

macro_rules! compare_lazy_eval_tests {
    ($($tests:tt)*) => {
        compare_eval_tests!(Strictness::Lazy, { $($tests)* });
    }
}

compare_strict_eval_tests! {
    // Wrap in a string since nix-instantiate formats store paths with quotes, but tvix does not.
    fetch_tarball(r#"[ "${(builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/91050ea1e57e50388fa87a3302ba12d188ef723a.tar.gz";
        sha256 = "1hf6cgaci1n186kkkjq106ryf8mmlq9vnwgfwh625wa8hfgdn4dm";
    })}" ]"#);
}

compare_lazy_eval_tests! {
    // Wrap every expression type supported by [Compiler::compile] in a list
    // with lazy evaluation enabled, so we can check it being thunked or not
    // against C++ Nix.
    thunked_fetch_tarball(r#"[ (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/91050ea1e57e50388fa87a3302ba12d188ef723a.tar.gz";
        sha256 = "1hf6cgaci1n186kkkjq106ryf8mmlq9vnwgfwh625wa8hfgdn4dm";
    }) ]"#);
}
