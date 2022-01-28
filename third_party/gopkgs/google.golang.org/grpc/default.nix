{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "google.golang.org/grpc";
    src =
      pkgs.fetchgit
        {
          url = "https://github.com/grpc/grpc-go";
          rev = "085c980048876e2735d4aba8f0d5bca4d7acaaa5";
          hash = "sha256:1vl089pv8qgxkbdg10kyd7203psn35wwjzxxbvi22628faqcpg61";
        };
    deps =
      with depot.third_party;
      [
        gopkgs."golang.org".x.net.trace
        gopkgs."golang.org".x.net.http2
        gopkgs."golang.org".x.net.http2.hpack
        gopkgs."golang.org".x.sys.unix
        gopkgs."github.com".golang.protobuf.proto
        gopkgs."github.com".golang.protobuf.ptypes
        gopkgs."github.com".golang.protobuf.ptypes.duration
        gopkgs."github.com".golang.protobuf.ptypes.timestamp
        gopkgs."google.golang.org".genproto.googleapis.rpc.status
      ];
  }
