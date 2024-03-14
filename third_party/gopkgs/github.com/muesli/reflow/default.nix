{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/muesli/reflow";
  src = pkgs.fetchFromGitHub {
    owner = "muesli";
    repo = "reflow";
    # unreleased version required by bubbletea
    rev = "9e1d0d53df68baf262851201166872afafd04e5d";
    sha256 = "08bmkqdn7sb5laqc1mvgk4xj31f600n1y04s1ifppjvszbcsxhid";
  };

  deps = with depot.third_party; [ gopkgs."github.com".mattn.go-runewidth ];
}
