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
    src = ./.;
    cargoDeps = crate2nix.allWorkspaceMembers;
    name = "tvix-daemon-clippy";

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
  meta.ci.targets = [
    "tvix-daemon"
    "shell"
  ];
}
