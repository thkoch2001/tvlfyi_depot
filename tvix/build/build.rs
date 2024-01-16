use core::panic;
use std::io::Result;

const SANDBOX_SHELL: &str = "TVIX_BUILD_SANDBOX_SHELL";

fn main() -> Result<()> {
    #[allow(unused_mut)]
    let mut builder = tonic_build::configure();

    #[cfg(feature = "tonic-reflection")]
    {
        let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
        let descriptor_path = out_dir.join("tvix.build.v1.bin");

        builder = builder.file_descriptor_set_path(descriptor_path);
    };

    // https://github.com/hyperium/tonic/issues/908
    let mut config = prost_build::Config::new();
    config.bytes(["."]);
    config.extern_path(".tvix.castore.v1", "::tvix_castore::proto");

    builder
        .build_server(true)
        .build_client(true)
        .emit_rerun_if_changed(false)
        .compile_with_config(
            config,
            &[
                "tvix/build/protos/build.proto",
                "tvix/build/protos/rpc_build.proto",
            ],
            // If we are in running `cargo build` manually, using `../..` works fine,
            // but in case we run inside a nix build, we need to instead point PROTO_ROOT
            // to a sparseTree containing that structure.
            &[match std::env::var_os("PROTO_ROOT") {
                Some(proto_root) => proto_root.to_str().unwrap().to_owned(),
                None => "../..".to_string(),
            }],
        )?;

    // expose SANDBOX_SHELL to rust builds on Linux.
    // We're more tolerant on other OS, as we don't have an implementation yet that needs it.
    #[cfg(target_os = "linux")]
    {
        // Bail out if the environment variable is not set.
        match std::env::var(SANDBOX_SHELL) {
            Err(_) => {
                panic!("set {} env var to a sandbox shell", SANDBOX_SHELL);
            }
            Ok(sandbox_shell_path) => {
                // Return an instruction to Cargo that will set the environment
                // variable during rustc calls.
                //
                // https://doc.rust-lang.org/cargo/reference/build-scripts.html#cargorustc-envvarvalue
                println!(
                    "cargo:rustc-env={}={}",
                    SANDBOX_SHELL,
                    sandbox_shell_path.trim()
                );
            }
        }
    }

    Ok(())
}
