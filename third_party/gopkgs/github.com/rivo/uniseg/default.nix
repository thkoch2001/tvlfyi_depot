{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/rivo/uniseg";
  src = pkgs.fetchFromGitHub {
    owner = "rivo";
    repo = "uniseg";
    rev = "v0.1.0";
    sha256 = "0flpc1px1l6b1lxzhdxi0mvpkkjchppvgxshxxnlmm40s76i9ww5";
  };

  deps = with depot.third_party; [ ];
}
