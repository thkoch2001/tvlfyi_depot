//! Build script that can be used outside of Nix builds to inject the
//! ATWARD_INDEX_HTML variable when building in development mode.
//!
//! Note that this script assumes that atward is in a checkout of the
//! TVL depot.

use std::process::Command;

static ATWARD_INDEX_HTML: &str = "ATWARD_INDEX_HTML";
static ERROR_MESSAGE: &str = r#"Failed to build index page.

When building during development, atward expects to be in a checkout
of the TVL depot. This is required to automatically build the index
page that is needed at compile time.

As atward can not automatically detect the location of the page,
you must set the `ATWARD_INDEX_HTML` environment variable to the
right path.

The expected page is build using the files in //web/atward/indexHtml
in the depot."#;

fn main() {
    // Do nothing if the variable is already set (e.g. via Nix)
    if let Ok(_) = std::env::var(ATWARD_INDEX_HTML) {
        return;
    }

    // Otherwise ask Nix to build it and inject the result.
    let output = Command::new("nix-build")
        .arg("-A").arg("web.atward.indexHtml")
        // ... assuming atward is at //web/atward ...
        .arg("../..")
        .output()
        .expect(ERROR_MESSAGE);

    if !output.status.success() {
        eprintln!("{}\nNix output: {}", ERROR_MESSAGE, String::from_utf8_lossy(&output.stderr));
        return;
    }

    let out_path = String::from_utf8(output.stdout)
        .expect("Nix returned invalid output after building index page");

    // Return an instruction to Cargo that will set the environment
    // variable during rustc calls.
    //
    // https://doc.rust-lang.org/cargo/reference/build-scripts.html#cargorustc-envvarvalue
    println!("cargo:rustc-env={}={}", ATWARD_INDEX_HTML, out_path.trim());
}
