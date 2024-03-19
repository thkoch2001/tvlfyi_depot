{ depot, lib, pkgs, ... }:

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
  meta.ci.targets = [
    "tvix-daemon"
    "shell"
  ];
}
