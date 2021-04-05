{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/lucasb-eyer/go-colorful";
  src = pkgs.fetchFromGitHub {
    owner = "lucasb-eyer";
    repo = "go-colorful";
    # unreleased version required by bubbletea
    rev = "v1.2.0";
    sha256 = "08c3fkf27r16izjjd4w94xd1z7w1r4mdalbl53ms2ka2j465s3qs";
  };

  deps = with depot.third_party; [
  ];
}
