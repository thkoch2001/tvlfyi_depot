{ depot, lib, pkgs, ... }:

let
  wasmRust = pkgs.rust-bin.stable.latest.default.override {
    targets = [ "wasm32-unknown-unknown" ];
  };

  cargoToml = with builtins; fromTOML (readFile ./Cargo.toml);

  wasmBindgenMatch =
    cargoToml.dependencies.wasm-bindgen == "= ${pkgs.wasm-bindgen-cli.version}";

  assertWasmBindgen = assert (lib.assertMsg wasmBindgenMatch ''
    Due to instability in the Rust WASM ecosystem, the trunk build
    tool enforces that the Cargo-dependency version of `wasm-bindgen`
    MUST match the version of the CLI supplied in the environment.

    This can get out of sync when nixpkgs is updated. To resolve it,
    wasm-bindgen must be bumped in the Cargo.toml file and cargo needs
    to be run to resolve the dependencies.

    Versions of `wasm-bindgen` in Cargo.toml:

      Expected: '= ${pkgs.wasm-bindgen-cli.version}'
      Actual:   '${cargoToml.dependencies.wasm-bindgen}'
  ''); pkgs.wasm-bindgen-cli;

  deps = [
    pkgs.binaryen
    pkgs.sass
    pkgs.trunk

    wasmRust
    assertWasmBindgen
  ];

  # Cargo.toml needs to be patched with the /nix/store source path of
  # tvix-eval.
  cargoTomlPatch = pkgs.writeText "tvix-eval-src.patch" ''
    diff --git a/Cargo.toml b/Cargo.toml
    index 2e6c793..67280e7 100644
    --- a/Cargo.toml
    +++ b/Cargo.toml
    @@ -18,5 +18,5 @@ git = "https://github.com/nix-community/rnix-parser.git"
     rev = "97b438e34be5211a4b48aeed9cc3ded489b4d6da"

     [dependencies.tvix-eval]
    -path = "../../tvix/eval"
    +path = "${depot.tvix.eval.src}"
     default-features = false
  '';

in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "tvixbolt";
  version = "canon";
  src = lib.cleanSource ./.;

  cargoLock.lockFile = ./Cargo.lock;
  cargoLock.outputHashes = {
    "rnix-0.11.0-dev" = "sha256:1dd8gf2v4v6451r3n9iaks7bqk55izgblkqzymx77advpqwlpymq";
  };

  patches = [
    cargoTomlPatch
  ];

  buildPhase = ''
    export PATH=${lib.makeBinPath deps}:$PATH
    mkdir home
    export HOME=$PWD/home
    trunk build --release -d $out
  '';

  dontInstall = true;
}
