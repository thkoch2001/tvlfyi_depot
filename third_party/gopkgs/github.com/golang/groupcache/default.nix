{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/golang/groupcache";

  src = pkgs.fetchgit {
    url = "https://github.com/golang/groupcache";
    rev = "611e8accdfc92c4187d399e95ce826046d4c8d73";
    hash = "sha256:0ydaq1xn03h2arfdri0vcv0df19pk8dvq4ly5hm1kv18yjfv1v13";
  };

  deps = with depot.third_party; [ gopkgs."github.com".golang.protobuf.proto ];
}
