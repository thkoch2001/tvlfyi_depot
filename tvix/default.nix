# Nix helpers for projects under //tvix
{ pkgs, lib, depot, ... }:

let
  # Load the crate2nix crate tree.
  crates = pkgs.callPackage ./Cargo.nix {
    defaultCrateOverrides = depot.tvix.utils.defaultCrateOverridesForPkgs pkgs;
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
        "wu-manber"
        "tracing-opentelemetry"
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

        ${pkgs.crate2nix}/bin/crate2nix generate --all-features
        ${pkgs.treefmt}/bin/treefmt Cargo.nix \
          --no-cache \
          --on-unmatched=debug \
          --config-file=${depot.tools.depotfmt.config} \
          --tree-root=.

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
