# Nix helpers for projects under //tvix
{ pkgs, depot, ... }:

let
  # crate override for crates that need protobuf
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.protobuf ];

  # Cargo dependencies to be used with nixpkgs rustPlatform functions.
  cargoDeps = pkgs.rustPlatform.importCargoLock {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "test-generator-0.3.0" = "08brp3qqa55hijc7xby3lam2cc84hvx1zzfqv6lj7smlczh8k32y";
      "tonic-mock-0.1.0" = "0lwa03hpp0mxa6aa1zv5w68k61y4hccfm0q2ykyq392fwal8vb50";
      "wu-manber-0.1.0" = "02byhfiw41mlgr1c43n2iq6jw5sbyn8l1acv5v71a07h5l18q0cy";
    };
  };
in
{
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

  # Build the Rust documentation for publishing on docs.tvix.dev. Currently only
  # some crates are documented, as the crates that depend on Protobuf cause
  # build failures.
  rust-docs = pkgs.stdenv.mkDerivation {
    inherit cargoDeps;
    name = "tvix-rust-docs";
    src = depot.third_party.gitignoreSource ./.;
    PROTO_ROOT = depot.tvix.store.protos;

    nativeBuildInputs = with pkgs; [
      cargo
      rust-analyzer
      rustPlatform.cargoSetupHook
      rustc
      protobuf
    ];

    buildPhase = ''
      cargo doc --document-private-items
      mv target/doc $out
    '';
  };

  meta.ci.targets = [ "shell" "rust-docs" ];
}
