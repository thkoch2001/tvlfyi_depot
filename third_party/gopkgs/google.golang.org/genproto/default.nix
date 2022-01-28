{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "google.golang.org/genproto";
    src =
      pkgs.fetchgit
        {
          url = "https://github.com/google/go-genproto";
          rev = "0243a4be9c8f1264d238fdc2895620b4d9baf9e1";
          hash = "sha256:071672lk0pzns98ncbqk6np7l9flwh84hjjibhhm2s1fi941m6q3";
        };
    deps =
      with depot.third_party;
      [ gopkgs."github.com".golang.protobuf.proto.gopkg gopkgs."github.com".golang.protobuf.ptypes.any.gopkg ];
  }
