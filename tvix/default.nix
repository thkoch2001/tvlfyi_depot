# Nix helpers for projects under //tvix
{ pkgs, lib, depot, ... }:

let
  # crate override for crates that need protobuf
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.protobuf ];
  iconvDarwinDep = lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ];
  # fuse-backend-rs uses DiskArbitration framework to handle mount/unmount on Darwin
  diskArbitrationDep = lib.optionals pkgs.stdenv.isDarwin [ pkgs.buildPackages.darwin.apple_sdk.frameworks.DiskArbitration ];

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
        nativeBuildInputs = prev.nativeBuildInputs or [ ] ++ iconvDarwinDep;
      };

      prost-build = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tonic-reflection = prev: {
        nativeBuildInputs = protobufDep prev;
      };

      tvix-castore = prev: {
        PROTO_ROOT = depot.tvix.proto;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-store = prev: {
        PROTO_ROOT = depot.tvix.proto;
        nativeBuildInputs = protobufDep prev;
        buildInputs = prev.buildInputs or [ ] ++ diskArbitrationDep;
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
      ]);
  };
in
{
  inherit crates;

  # Run crate2nix generate in the current working directory, then
  # format the generated file with depotfmt.
  crate2nixGenerate = pkgs.writeShellScriptBin "crate2nix-generate" ''
    ${pkgs.crate2nix}/bin/crate2nix generate --all-features
    ${depot.tools.depotfmt}/bin/depotfmt Cargo.nix
  '';

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

  # Builds and tests the code in castore/protos.
  castore-protos-go = pkgs.buildGoModule {
    name = "castore-golang";
    src = depot.third_party.gitignoreSource ./store/protos;
    vendorHash = "sha256-o7moXRxhKxCpsds96sSsHHafKJf2AWhFMu/YdSu+FM4=";
  };

  # Builds and tests the code in store/protos.
  store-protos-go = pkgs.buildGoModule {
    name = "store-golang";
    src = depot.third_party.gitignoreSource ./store/protos;
    vendorHash = "sha256-o7moXRxhKxCpsds96sSsHHafKJf2AWhFMu/YdSu+FM4=";
  };

  # Build the Rust documentation for publishing on docs.tvix.dev.
  rust-docs = pkgs.stdenv.mkDerivation {
    inherit cargoDeps;
    name = "tvix-rust-docs";
    src = depot.third_party.gitignoreSource ./.;
    PROTO_ROOT = depot.tvix.proto;

    buildInputs = [
      pkgs.fuse
    ];
    nativeBuildInputs = with pkgs; [
      cargo
      pkg-config
      protobuf
      rustc
      rustPlatform.cargoSetupHook
    ] ++ iconvDarwinDep;

    buildPhase = ''
      cargo doc --document-private-items
      mv target/doc $out
    '';
  };

  meta.ci.targets = [
    "castore-protos-go"
    "store-protos-go"
    "shell"
    "rust-docs"
  ];
}
