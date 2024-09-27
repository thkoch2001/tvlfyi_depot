# Nix helpers for projects under //tvix
{ pkgs, lib, depot, ... }:

let
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

      opentelemetry-proto = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      prost-build = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      prost-wkt-types = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tonic-reflection = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tvix-build = prev: {
        src = filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = (lib.fileset.fileFilter (f: f.hasExt "proto") root);
        };
        PROTO_ROOT = depot.tvix.build.protos.protos;
        TVIX_BUILD_SANDBOX_SHELL = "${pkgs.busybox-sandbox-shell}/bin/busybox";
        nativeBuildInputs = protobufDep prev;
        buildInputs = darwinDeps;
      };

      tvix-castore = prev: {
        src = filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = (lib.fileset.fileFilter (f: f.hasExt "proto") root);
        };
        PROTO_ROOT = depot.tvix.castore.protos.protos;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-cli = prev: {
        src = filterRustCrateSrc { root = prev.src.origSrc; };
        buildInputs = prev.buildInputs or [ ] ++ darwinDeps;
      };

      tvix-store = prev: {
        src = filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = (lib.fileset.fileFilter (f: f.hasExt "proto") root);
        };
        PROTO_ROOT = depot.tvix.store.protos.protos;
        nativeBuildInputs = protobufDep prev;
        # fuse-backend-rs uses DiskArbitration framework to handle mount/unmount on Darwin
        buildInputs = prev.buildInputs or [ ]
          ++ darwinDeps
          ++ lib.optional pkgs.stdenv.isDarwin pkgs.buildPackages.darwin.apple_sdk.frameworks.DiskArbitration;
      };

      tvix-eval-builtin-macros = prev: {
        src = filterRustCrateSrc { root = prev.src.origSrc; };
      };

      tvix-eval = prev: {
        src = filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = (root + "/proptest-regressions");
        };
      };

      tvix-glue = prev: {
        src = filterRustCrateSrc {
          root = prev.src.origSrc;
        };
      };

      tvix-serde = prev: {
        src = filterRustCrateSrc { root = prev.src.origSrc; };
      };

      tvix-tracing = prev: {
        src = filterRustCrateSrc { root = prev.src.origSrc; };
      };

      nix-compat = prev: {
        src = filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = (root + "/testdata");
        };
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
        (k:
          (lib.nameValuePair "${crates.internal.crates.${k}.crateName}-${crates.internal.crates.${k}.version}" crates.internal.crates.${k}.src.outputHash)
        ) [
        "bigtable_rs"
        "wu-manber"
      ]);
  };

  # The cleaned sources.
  src = depot.third_party.gitignoreSource ./.;

  # Target containing *all* tvix proto files.
  # Useful for workspace-wide cargo invocations (doc, clippy)
  protos = pkgs.symlinkJoin {
    name = "tvix-all-protos";
    paths = [
      depot.tvix.build.protos.protos
      depot.tvix.castore.protos.protos
      depot.tvix.store.protos.protos
    ];
  };

in
{
  inherit crates protos;

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
    PROTO_ROOT = protos;
    TVIX_BUILD_SANDBOX_SHELL = "/homeless-shelter";

    nativeBuildInputs = with pkgs; [
      cargo
      pkg-config
      protobuf
      rustc
      rustPlatform.cargoSetupHook
    ];

    buildInputs = [
      pkgs.fuse
    ] ++ lib.optional pkgs.stdenv.isDarwin pkgs.libiconv;

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
    PROTO_ROOT = protos;
    TVIX_BUILD_SANDBOX_SHELL = "/homeless-shelter";

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

    buildPhase = "cargo clippy --tests --all-features --benches --examples -- -Dwarnings | tee $out";
  };

  crate2nix-check =
    let
      crate2nix-check = depot.tvix.utils.mkCrate2nixCheck ./Cargo.nix;
    in
    crate2nix-check.command.overrideAttrs {
      meta.ci.extraSteps = {
        inherit crate2nix-check;
      };
    };

  meta.ci.targets = [
    "clippy"
    "shell"
    "rust-docs"
    "crate2nix-check"
  ];

  utils = import ./utils.nix { inherit pkgs lib depot; };
}
