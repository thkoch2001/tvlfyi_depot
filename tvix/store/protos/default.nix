{ depot, pkgs, ... }: {
  # Produces the golang bindings.
  go-bindings = pkgs.stdenv.mkDerivation {
    name = "go-bindings";

    src = depot.nix.sparseTree {
      name = "castore-protos";
      root = depot.path.origSrc;
      paths = [
        # We need to include castore.proto (only), as it's referred.
        ../../castore/protos/castore.proto
        ./pathinfo.proto
        ./rpc_pathinfo.proto
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
      buf generate

      mkdir -p $out
      cp tvix/store/protos/*.pb.go $out/
    '';
  };
}
