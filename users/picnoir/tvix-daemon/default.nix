{ depot, pkgs, ... }:

let
  crate2nix = pkgs.callPackage ./Cargo.nix {
    defaultCrateOverrides = {
      tvix-castore = prev: {
        PROTO_ROOT = depot.tvix.castore.protos.protos;
        nativeBuildInputs = protobufDep prev;
      };

      tvix-store = prev: {
        PROTO_ROOT = depot.tvix.store.protos.protos;
        nativeBuildInputs = protobufDep prev;
      };
    };
  };
  protobufDep = prev: (prev.nativeBuildInputs or [ ]) ++ [ pkgs.buildPackages.protobuf ];
in
{
  shell = (import ./shell.nix { inherit pkgs; });
  tvix-daemon = crate2nix.rootCrate.build;
  clippy = pkgs.stdenv.mkDerivation {
    name = "tvix-daemon-clippy";
    # The cleaned sources.
    src = depot.third_party.gitignoreSource ./.;
    cargoDeps = crate2nix.allWorkspaceMembers;

    nativeBuildInputs = with pkgs; [
      cargo
      clippy
      pkg-config
      protobuf
      rustc
      rustPlatform.cargoSetupHook
    ];

    buildPhase = "cargo clippy --tests --all-features --benches --examples | tee $out";
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
    "tvix-daemon"
    "shell"
    "crate2nix-check"
  ];
}
