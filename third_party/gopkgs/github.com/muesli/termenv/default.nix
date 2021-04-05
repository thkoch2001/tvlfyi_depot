{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/muesli/termenv";
  src = pkgs.fetchFromGitHub {
    owner = "muesli";
    repo = "termenv";
    # unreleased version required by bubbletea
    rev = "v0.8.1";
    sha256 = "0m24ljq1nq7z933fcvg99fw0fhxj9rb5ll4rlay7z2f2p59mrbdp";
  };

  deps = with depot.third_party; [
    gopkgs."github.com".lucasb-eyer.go-colorful
    gopkgs."github.com".mattn.go-isatty
    gopkgs."github.com".mattn.go-runewidth
    gopkgs."golang.org".x.sys.unix
  ];
}
