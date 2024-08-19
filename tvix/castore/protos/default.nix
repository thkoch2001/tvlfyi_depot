<<<<<<< Updated upstream
{ depot, lib, pkgs, ... }:
=======
{ depot, pkgs, lib, ... }:
>>>>>>> Stashed changes
let
  protos = lib.sourceByRegex depot.path.origSrc [
    "buf.yaml"
    "buf.gen.yaml"
<<<<<<< Updated upstream
    "tvix/.*/(castore|rpc_blobstore|rpc_directory)\.proto"
=======
    "^tvix(/castore(/protos(/.*\.proto)?)?)?$"
>>>>>>> Stashed changes
  ];
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
      cp tvix/castore/protos/*.pb.go $out/
    '';
  };
}
