# some change to cause a different object to exist

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
in
pkgs.rustPlatform.buildRustPackage rec {
  pname = "pwcrypt";
  version = "canon";
  src = lib.cleanSource ./.;
  cargoLock.lockFile = ./Cargo.lock;

  buildPhase = ''
    export PATH=${lib.makeBinPath deps}:$PATH
    mkdir home
    export HOME=$PWD/home
    trunk build --release -d $out
  '';

  dontInstall = true;
}
