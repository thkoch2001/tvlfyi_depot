<<<<<<< Updated upstream
{ depot, lib, pkgs, ... }:
=======
{ depot, pkgs, lib, ... }:
>>>>>>> Stashed changes
let
  protos = lib.sourceByRegex depot.path.origSrc [
    "buf.yaml"
    "buf.gen.yaml"
    # We need to include castore.proto (only), as it's referred.
<<<<<<< Updated upstream
    "tvix/.*/(castore|build|rpc_build)\.proto"
=======
    "^tvix(/castore(/protos(/castore\.proto)?)?)?$"
    "^tvix(/build(/protos(/.*\.proto)?)?)?$"
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
      cp tvix/build/protos/*.pb.go $out/
    '';
  };
}
