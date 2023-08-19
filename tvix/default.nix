# Nix helpers for projects under //tvix
{ pkgs, depot, ... }:

let
  # crate override for crates that need protobuf
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.protobuf ];

  # Load the crate2nix crate tree.
  crates = import ./Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;

    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      fuser = prev: {
        buildInputs = prev.buildInputs or [ ] ++ [ pkgs.fuse ];
        nativeBuildInputs = prev.nativeBuildInputs or [ ] ++ [ pkgs.pkg-config ];
      };

      prost-build = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tonic-reflection = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tvix-store = prev: {
        PROTO_ROOT = depot.tvix.store.protos;
        nativeBuildInputs = protobufDep prev;
      };
    };
  };

  # Cargo dependencies to be used with nixpkgs rustPlatform functions.
  cargoDeps = pkgs.rustPlatform.importCargoLock {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "test-generator-0.3.0" = crates.internal.crates.test-generator.src.outputHash;
      "tonic-mock-0.1.0" = crates.internal.crates.tonic-mock.src.outputHash;
      "wu-manber-0.1.0" = crates.internal.crates.wu-manber.src.outputHash;
    };
  };
in
{
  inherit crates;

  # Run crate2nix generate in the current working directory, then
  # format the generated file with depotfmt.
  crate2nixGenerate = pkgs.writeShellScriptBin "crate2nix-generate" ''
    ${pkgs.crate2nix}/bin/crate2nix generate
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  '';

  # Provide a shell for the combined dependencies of all Tvix Rust
  # projects. Note that as this is manually maintained it may be
  # lacking something, but it is required for some people's workflows.
  #
  # This shell can be entered with e.g. `mg shell //tvix:shell`.
  shell = pkgs.mkShell {
    name = "tvix-rust-dev-env";
    packages = [
      pkgs.buf-language-server
      pkgs.cargo
      pkgs.clippy
      pkgs.evans
      pkgs.fuse
      pkgs.pkg-config
      pkgs.rust-analyzer
      pkgs.rustc
      pkgs.rustfmt
      pkgs.protobuf
    ];
  };

  # Build the Rust documentation for publishing on docs.tvix.dev.
  rust-docs = pkgs.stdenv.mkDerivation {
    inherit cargoDeps;
    name = "tvix-rust-docs";
    src = depot.third_party.gitignoreSource ./.;
    PROTO_ROOT = depot.tvix.store.protos;

    buildInputs = [
      pkgs.fuse
    ];
    nativeBuildInputs = with pkgs; [
      cargo
      pkg-config
      protobuf
      rust-analyzer
      rustc
      rustPlatform.cargoSetupHook
    ];

    buildPhase = ''
      cargo doc --document-private-items
      mv target/doc $out
    '';
  };

  meta.ci.targets = [ "shell" "rust-docs" ];
}
