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

    meta.ci.extraSteps.deploy = {
      label = ":rocket: deploy";
      needsOutput = true;
      alwaysRun = true;
      prompt = "Deploy rust-docs?";
      phase = "release";
      # branches = ["canon"];
      command = pkgs.writeShellScript "deploy-tvix-rust-docs" ''
        RCLONE_WEBDAV_URL="https://api.garage.flokli.io/tvix-rust-docs" \
        RCLONE_WEBDAV_BEARER_TOKEN_COMMAND="${pkgs.buildkite-agent}/bin/buildkite-agent oidc request-token --audience api.garage.flokli.io" \
        ${pkgs.rclone}/bin/rclone sync result/. :webdav:
      '';
    };
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
