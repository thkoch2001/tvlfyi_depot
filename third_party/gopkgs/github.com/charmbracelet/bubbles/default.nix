{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/charmbracelet/bubbles";
  src = pkgs.fetchFromGitHub {
    owner = "charmbracelet";
    repo = "bubbles";
    # unreleased version required by bubbletea
    rev = "v0.7.6";
    sha256 = "1gd4k4f2mj2dnqcbpdrh9plziz0l29ls6mgyy4mfdcdfijfyd30n";
  };

  deps = with depot.third_party;
    [ gopkgs."github.com".charmbracelet.bubbletea ];
}
