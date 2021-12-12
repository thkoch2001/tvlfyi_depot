{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/mattn/go-runewidth";
  src = pkgs.fetchFromGitHub {
    owner = "mattn";
    repo = "go-runewidth";
    rev = "v0.0.10";
    sha256 = "0jh9552ppqvkdfni7x623n0x5mbiaqqhjhmr0zkh28x56k4ysii4";
  };

  deps = with depot.third_party; [ gopkgs."github.com".rivo.uniseg ];
}
