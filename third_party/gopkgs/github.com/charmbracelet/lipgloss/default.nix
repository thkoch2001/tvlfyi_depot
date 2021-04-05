{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/charmbracelet/lipgloss";
  src = pkgs.fetchFromGitHub {
    owner = "charmbracelet";
    repo = "lipgloss";
    # unreleased version required by bubbletea
    rev = "v0.1.0";
    sha256 = "1chhs492rsq7i4mr6qpjv3d89rvsd23ri6psnmil3ah6i286vl06";
  };

  deps = with depot.third_party; [
    # gopkgs."github.com".charmbracelet.bubbletea
    gopkgs."github.com".lucasb-eyer.go-colorful
    gopkgs."github.com".muesli.reflow.ansi
    gopkgs."github.com".muesli.reflow.truncate
    gopkgs."github.com".muesli.reflow.wordwrap
    gopkgs."github.com".muesli.termenv
  ];
}
