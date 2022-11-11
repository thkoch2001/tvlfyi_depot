{ depot, pkgs, lib, ... }:

let
  protoRoot = depot.nix.sparseTree depot.path.origSrc [
    ./protos/castore.proto
    ./protos/pathinfo.proto
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
