{ depot, pkgs, ... }: {
  # Produces the golang bindings.
  go-bindings = pkgs.stdenv.mkDerivation {
    name = "go-bindings";

    src = depot.nix.sparseTree {
      name = "build-protos";
      root = depot.path.origSrc;
      paths = [
        # We need to include castore.proto (only), as it's referred.
        ../../castore/protos/castore.proto
        ./build.proto
        ./rpc_build.proto
        ../../../buf.yaml
        ../../../buf.gen.yaml
      ];
    };

    nativeBuildInputs = [
      pkgs.buf
      pkgs.protoc-gen-go
      pkgs.protoc-gen-go-grpc
    ];

    buildPhase = ''
      export HOME=$TMPDIR
      buf lint
      buf format -d --exit-code
      buf generate

      mkdir -p $out
      cp tvix/build/protos/*.pb.go $out/
    '';
  };
}
