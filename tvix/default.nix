# Nix helpers for projects under //tvix
{ pkgs
, lib ? pkgs.lib
, depot ? null
, tvix ? depot.tvix
, ... }:

let
  PROTO_ROOT = tvix.proto;
  cleanSource = if depot == null then lib.cleanSource else depot.third_party.gitignoreSource;

  # crate override for crates that need protobuf
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.buildPackages.protobuf ];
  iconvDarwinDep = lib.optional pkgs.stdenv.isDarwin pkgs.libiconv;

  # On Darwin, some crates producing binaries need to be able to link against security.
  securityDarwinDep = lib.optional pkgs.stdenv.isDarwin pkgs.buildPackages.darwin.apple_sdk.frameworks.Security;

  # Load the crate2nix crate tree.
  crates = import ./Cargo.nix {
    inherit pkgs;
    nixpkgs = pkgs.path;

    # Hack to fix Darwin build
    # See https://github.com/NixOS/nixpkgs/issues/218712
    buildRustCrateForPkgs = pkgs:
      if pkgs.stdenv.isDarwin then
        let
          buildRustCrate = pkgs.buildRustCrate;
          buildRustCrate_ = args: buildRustCrate args // { dontStrip = true; };
          override = o: args: buildRustCrate.override o (args // { dontStrip = true; });
        in
        pkgs.makeOverridable override { }
      else pkgs.buildRustCrate;

    defaultCrateOverrides = pkgs.defaultCrateOverrides // {
      zstd-sys = prev: {
        nativeBuildInputs = prev.nativeBuildInputs or [ ];
        buildInputs = prev.buildInputs or [ ] ++ iconvDarwinDep;
      };

      prost-build = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tonic-reflection = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tvix-castore = prev: {
        inherit PROTO_ROOT;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-cli = prev: {
        buildInputs = prev.buildInputs or [ ] ++ securityDarwinDep;
      };

      tvix-store = prev: {
        inherit PROTO_ROOT;
        nativeBuildInputs = protobufDep prev;
        # fuse-backend-rs uses DiskArbitration framework to handle mount/unmount on Darwin
        buildInputs = prev.buildInputs or [ ]
          ++ securityDarwinDep
          ++ lib.optional pkgs.stdenv.isDarwin pkgs.buildPackages.darwin.apple_sdk.frameworks.DiskArbitration;
      };
    };
  };

  # Cargo dependencies to be used with nixpkgs rustPlatform functions.
  cargoDeps = pkgs.rustPlatform.importCargoLock {
    lockFile = ./Cargo.lock;
    # Extract the hashes from `crates` / Cargo.nix, we already get them from cargo2nix.
    # This returns an attribute set containing "${crateName}-${version}" as key,
    # and the outputHash as value.
    outputHashes = builtins.listToAttrs
      (map
        (crateName:
          (lib.nameValuePair "${crateName}-${crates.internal.crates.${crateName}.version}" crates.internal.crates.${crateName}.src.outputHash)
        ) [
        "fuse-backend-rs"
        "test-generator"
        "wu-manber"

        "futures-channel"
        "futures-core"
        "futures-executor"
        "futures-io"
        "futures-macro"
        "futures-sink"
        "futures-task"
        "futures-util"
      ]);
  };

  # The cleaned sources.
  src = cleanSource ./.;

in
rec {
  inherit crates;

  # Run crate2nix generate in the current working directory, then
  # format the generated file with depotfmt.
  crate2nixGenerate = pkgs.writeShellScriptBin "crate2nix-generate" ''
    export CARGO_HOME=$(mktemp -d)
    ${pkgs.crate2nix}/bin/crate2nix generate --all-features
  '' + lib.optionalString (depot != null) ''
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  '';

  # Run crate2nix generate, ensure the output doesn't differ afterwards
  # (and doesn't fail)
  crate2nix-check = pkgs.stdenv.mkDerivation {
    inherit src;
    name = "tvix-crate2nix-check";
    nativeBuildInputs = [ crate2nixGenerate ];
    buildPhase = ''
      crate2nix-generate
      diff -ur . ${src}
      touch $out
    '';
  };

  # Provide the Tvix logo in both .webp and .png format.
  logo = pkgs.runCommand "logo"
    {
      nativeBuildInputs = [ pkgs.imagemagick ];
    } ''
    mkdir -p $out
    cp ${./logo.webp} $out/logo.webp
    convert $out/logo.webp $out/logo.png
  '';

  # Provide a shell for the combined dependencies of all Tvix Rust
  # projects. Note that as this is manually maintained it may be
  # lacking something, but it is required for some people's workflows.
  #
  # This shell can be entered with e.g. `mg shell //tvix:shell`.
  # This is a separate file, so it can be used individually in the tvix josh
  # workspace too.
  shell = (import ./shell.nix { inherit pkgs; });

  # Build the Rust documentation for publishing on docs.tvix.dev.
  rust-docs = pkgs.stdenv.mkDerivation {
    inherit cargoDeps src;
    name = "tvix-rust-docs";
    inherit PROTO_ROOT;

    nativeBuildInputs = with pkgs; [
      cargo
      pkg-config
      protobuf
      rustc
      rustPlatform.cargoSetupHook
    ];

    buildInputs = [
      pkgs.fuse
    ] ++ iconvDarwinDep;

    buildPhase = ''
      cargo doc --document-private-items
      mv target/doc $out
    '';
  };

  # Run cargo clippy. We run it with -Dwarnings, so warnings cause a nonzero
  # exit code.
  clippy = pkgs.stdenv.mkDerivation {
    inherit cargoDeps src;
    name = "tvix-clippy";
    inherit PROTO_ROOT;

    buildInputs = [
      pkgs.fuse
    ];
    nativeBuildInputs = with pkgs; [
      cargo
      clippy
      pkg-config
      protobuf
      rustc
      rustPlatform.cargoSetupHook
    ];

    buildPhase = "cargo clippy -- -Dwarnings | tee $out";
  };

  meta.ci.targets = [
    "clippy"
    "crate2nix-check"
    "shell"
    "rust-docs"
  ];
}
