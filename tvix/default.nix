# Nix helpers for projects under //tvix
{ pkgs, lib, depot, ... }:

let
  # crate override for crates that need protobuf
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.buildPackages.protobuf ];

  # On Darwin, some crates producing binaries need to be able to link against security.
  darwinDeps = lib.optionals pkgs.stdenv.isDarwin (with pkgs.buildPackages.darwin.apple_sdk.frameworks; [
    Security
    SystemConfiguration
  ]);

  defaultCrateOverridesForPkgs = pkgs: pkgs.defaultCrateOverrides // {
    zstd-sys = prev: {
      nativeBuildInputs = prev.nativeBuildInputs or [ ];
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
      src = depot.tvix.utils.filterRustCrateSrc rec {
        root = prev.src.origSrc;
        extraFileset = (lib.fileset.fileFilter (f: f.hasExt "proto") root);
      };
      PROTO_ROOT = depot.tvix.build.protos.protos;
      nativeBuildInputs = protobufDep prev;
      buildInputs = darwinDeps;
    };

    tvix-castore = prev: {
      src = depot.tvix.utils.filterRustCrateSrc rec {
        root = prev.src.origSrc;
        extraFileset = (lib.fileset.fileFilter (f: f.hasExt "proto") root);
      };
      PROTO_ROOT = depot.tvix.castore.protos.protos;
      nativeBuildInputs = protobufDep prev;
    };

    tvix-cli = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      buildInputs = prev.buildInputs or [ ] ++ darwinDeps;
    };

    tvix-store = prev: {
      src = depot.tvix.utils.filterRustCrateSrc rec {
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
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };

    tvix-eval = prev: {
      src = depot.tvix.utils.filterRustCrateSrc rec {
        root = prev.src.origSrc;
        extraFileset = (root + "/proptest-regressions");
      };
    };

    tvix-glue = prev: {
      src = depot.tvix.utils.filterRustCrateSrc {
        root = prev.src.origSrc;
      };
    };

    tvix-serde = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };

    tvix-tracing = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
    };

    nix-compat = prev: {
      src = depot.tvix.utils.filterRustCrateSrc rec {
        root = prev.src.origSrc;
        extraFileset = (root + "/testdata");
      };
    };

    tvixbolt = prev: {
      src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      installPhase = ''
        ${lib.getExe pkgs.pkgsBuildBuild.wasm-bindgen-cli} \
          --target web \
          --out-dir $out \
          --out-name ${prev.crateName} \
          --no-typescript \
          target/lib/${prev.crateName}-*.wasm
          cp src/{*.html,*.css} $out/
      '';
    };
  };

  # Load the crate2nix crate tree.
  crates = lib.recursiveUpdate
    (pkgs.callPackage ./Cargo.nix { defaultCrateOverrides = defaultCrateOverridesForPkgs pkgs; })
    {
      workspaceMembers =
        let
          nixpkgs-patched = (pkgs.applyPatches {
            name = "nixpkgs-patched";
            src = pkgs.path;
            patches = [
              (pkgs.fetchpatch {
                url = "https://github.com/NixOS/nixpkgs/commit/de9a49c390e6793af604c9a05f8c7015aff32903.patch";
                hash = "sha256-r/K3z/cVuwtuTdmqnAZ63gaSiH4BW2SFR4YA/oYgJao=";
              })
              (pkgs.fetchpatch {
                url = "https://github.com/NixOS/nixpkgs/commit/9555aee82eb332e65edee057bbe1f9f5a5083295.patch";
                hash = "sha256-9Pa7KjEVgTbpXBtDG9G+PNr1tmqrPLl7dNHIG+PnrYw=";
              })
              (pkgs.fetchpatch {
                url = "https://github.com/NixOS/nixpkgs/commit/33d0aed3c8e53b42026d76790f0dbb699e6930ae.patch";
                hash = "sha256-aucNkLajS+W6jTcoQ1VT5gOn06uFmARNEz7rrdgEsnk=";
              })
            ];
          });
          pkgsWasm32 = import nixpkgs-patched { crossSystem = (import (nixpkgs-patched + "/lib")).systems.examples.wasm32-unknown-none; };
        in
        {
          inherit ((pkgsWasm32.callPackage ./Cargo.nix {
            defaultCrateOverrides = defaultCrateOverridesForPkgs pkgsWasm32;
          }).workspaceMembers) tvixbolt;
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

  # Run crate2nix generate, ensure the output doesn't differ afterwards
  # (and doesn't fail).
  #
  # Currently this re-downloads every crate every time
  # crate2nix-check (but not crate2nix) is built.
  # TODO(amjoseph): be less wasteful with bandwidth.
  #
  crate2nix-check =
    let
      outputHashAlgo = "sha256";
    in
    pkgs.stdenv.mkDerivation {
      inherit src;

      # Important: we include the hash of all Cargo related files in the derivation name.
      # This forces the FOD to be rebuilt/re-verified whenever one of them changes.
      name = "tvix-crate2nix-check-" + builtins.substring 0 8 (builtins.hashString "sha256"
        (lib.concatMapStrings (f: builtins.hashFile "sha256" f)
          ([ ./Cargo.toml ./Cargo.lock ] ++ (map (m: ./. + "/${m}/Cargo.toml") (lib.importTOML ./Cargo.toml).workspace.members))
        )
      );

      nativeBuildInputs = with pkgs; [ git cacert cargo ];
      buildPhase = ''
        export CARGO_HOME=$(mktemp -d)

        # The following command can be omitted, in which case
        # crate2nix-generate will run it automatically, but won't show the
        # output, which makes it look like the build is somehow "stuck" for a
        # minute or two.
        cargo metadata > /dev/null

        # running this command counteracts depotfmt brokenness
        git init

        ${depot.tools.crate2nix-generate}/bin/crate2nix-generate

        # technically unnecessary, but provides more-helpful output in case of error
        diff -ur Cargo.nix ${src}/Cargo.nix

        # the FOD hash will check that the (re-)generated Cargo.nix matches the committed Cargo.nix
        cp Cargo.nix $out
      '';

      # This is an FOD in order to allow `cargo` to perform network access.
      outputHashMode = "flat";
      inherit outputHashAlgo;
      outputHash = builtins.hashFile outputHashAlgo ./Cargo.nix;
      env.SSL_CERT_FILE = "${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt";
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
    PROTO_ROOT = protos;

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

  meta.ci.targets = [
    "clippy"
    "crate2nix-check"
    "shell"
    "rust-docs"
  ];

  utils = import ./utils.nix { inherit lib depot; };
}
