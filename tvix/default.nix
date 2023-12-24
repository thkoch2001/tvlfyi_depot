# Nix helpers for projects under //tvix
{ pkgs, lib, depot, ... }:

let
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

      tvix-build = prev: {
        PROTO_ROOT = depot.tvix.build.protos.protos;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-castore = prev: {
        PROTO_ROOT = depot.tvix.castore.protos.protos;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-cli = prev: {
        buildInputs = prev.buildInputs or [ ] ++ securityDarwinDep;
      };

      tvix-store = prev: {
        PROTO_ROOT = depot.tvix.store.protos.protos;
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
        "test-generator"
        "wu-manber"
      ]);
  };

  # The cleaned sources.
  src = depot.third_party.gitignoreSource ./.;

  # Run crate2nix generate in the current working directory, then
  # format the generated file with depotfmt.
  crate2nix-generate = pkgs.writeShellScriptBin "crate2nix-generate" ''
    ${pkgs.crate2nix}/bin/crate2nix generate --all-features
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  '';

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
  inherit crates crate2nix-generate protos;

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

      # Important: we include the hash of the Cargo.lock file and
      # Cargo.nix file in the derivation name.  This forces the FOD
      # to be rebuilt/reverified whenever either of them changes.
      name = "tvix-crate2nix-check-" +
        (builtins.substring 0 8 (builtins.hashFile "sha256" ./Cargo.lock)) +
        "-" +
        (builtins.substring 0 8 (builtins.hashFile "sha256" ./Cargo.nix));

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

        ${crate2nix-generate}/bin/crate2nix-generate

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

    buildPhase = "cargo clippy --tests --all-features -- -Dwarnings | tee $out";
  };

  meta.ci.targets = [
    "clippy"
    "crate2nix-check"
    "shell"
    "rust-docs"
  ];
}
