{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "github.com/golang/protobuf";
    src =
      pkgs.fetchgit
        {
          url = "https://github.com/golang/protobuf";
          rev = "ed6926b37a637426117ccab59282c3839528a700";
          hash = "sha256:0fynqrim022x9xi2bivkw19npbz4316v4yr7mb677s9s36z4dc4h";
        };
  }
