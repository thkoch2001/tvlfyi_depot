{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "golang.org/x/crypto";

  src = pkgs.fetchgit {
    url = "https://go.googlesource.com/crypto";
    rev = "e9b2fee46413994441b28dfca259d911d963dfed";
    hash = "sha256:18sz5426h320l9gdll9n43lzzxg2dmqv0s5fjy6sbvbkkpjs1m28";
  };

  deps = with depot.third_party; [ gopkgs."golang.org".x.sys.unix.gopkg ];
}
