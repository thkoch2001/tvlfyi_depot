{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/charmbracelet/bubbletea";
  src = let
    gitSrc = pkgs.fetchFromGitHub {
      owner = "charmbracelet";
      repo = "bubbletea";
      rev = "v0.13.1";
      sha256 = "0yf2fjkvx8ym9n6f3qp2z7sxs0qsfpj148sfvbrp38k67s3h20cs";
    };
    # The examples/ directory is fairly extensive,
    # but it also adds most of the dependencies.
  in pkgs.runCommand gitSrc.name { } ''
    mkdir -p $out
    ln -s "${gitSrc}"/* $out
    rm -r $out/examples
    rm -r $out/tutorials
  '';
  deps = with depot.third_party; [
    gopkgs."github.com".containerd.console
    gopkgs."github.com".mattn.go-isatty
    gopkgs."github.com".muesli.reflow.truncate
    gopkgs."github.com".muesli.termenv
    gopkgs."golang.org".x.sys.unix
    gopkgs."golang.org".x.crypto.ssh.terminal
  ];
}
