{ depot, pkgs, lib, ... }:

let
  protoRoot = depot.nix.sparseTree depot.path.origSrc [
    ./tvix/store/protos/castore.proto
    ./tvix/store/protos/pathinfo.proto
    ./tvix/store/protos/rpc_blobstore.proto
    ./tvix/store/protos/rpc_directory.proto
    ./tvix/store/protos/rpc_pathinfo.proto
  ];
in
lib.fix (self: depot.third_party.naersk.buildPackage (lib.fix (naerskArgs: {
  src = depot.third_party.gitignoreSource ./.;
  # see https://github.com/nix-community/naersk/issues/169
  root = depot.nix.sparseTree ./. [ ./Cargo.lock ./Cargo.toml ];

  nativeBuildInputs = [ pkgs.protobuf ];

  PROTO_ROOT = protoRoot;

  doCheck = true;
}))
)
