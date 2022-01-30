{ depot, ... }:
depot.nix.buildGo.program {
  name = "struct-edit";
  srcs = [
    ./main.go
  ];
  deps = [
    depot.third_party.gopkgs."github.com".charmbracelet.bubbletea
    depot.third_party.gopkgs."github.com".charmbracelet.lipgloss
    depot.third_party.gopkgs."github.com".muesli.termenv
    depot.third_party.gopkgs."github.com".mattn.go-isatty
  ];
}
