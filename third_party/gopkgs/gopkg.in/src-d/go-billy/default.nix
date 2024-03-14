{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "gopkg.in/src-d/go-billy.v4";

  src = pkgs.fetchFromGitHub {
    owner = "src-d";
    repo = "go-billy";
    rev = "fd409ff12f33d0d60af0ce0abeb8d93df360af49";
    sha256 = "1j0pl6ggzmd2lrqj71vmsnl6cqm43145h7yg6sy3j5n7hhd592qv";
  };

  deps = with depot.third_party; [ gopkgs."golang.org".x.sys.unix ];
}
