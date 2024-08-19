{ depot, lib, pkgs, ... }:
let
  protos = lib.fileset.toSource {
    root = depot.path.origSrc;
    fileset = (lib.fileset.unions [
      # We need to include castore.proto (only), as it's referred.
      ../../castore/protos/castore.proto
      ./pathinfo.proto
      ./rpc_pathinfo.proto
      ../../../buf.yaml
      ../../../buf.gen.yaml
    ]);
  };
in
depot.nix.readTree.drvTargets {
  inherit protos;

  # Lints and ensures formatting of the proto files.
  check = pkgs.stdenv.mkDerivation {
    name = "proto-check";
    src = protos;

    nativeBuildInputs = [
      pkgs.buf
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      buf lint
      buf format -d --exit-code
      touch $out
    '';
  };

  # Produces the golang bindings.
  go-bindings = pkgs.stdenv.mkDerivation {
    name = "go-bindings";
    src = protos;

    nativeBuildInputs = [
      pkgs.buf
      pkgs.protoc-gen-go
      pkgs.protoc-gen-go-grpc
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      buf generate

      mkdir -p $out
      cp tvix/store/protos/*.pb.go $out/
    '';
  };
}
