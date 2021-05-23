{ pkgs, ... }:

pkgs.stdenvNoCC.mkDerivation {
  name = "deploy-whitby";

  unpackPhase = ":";

  buildPhase = ''
    mkdir -p $out/bin
    makeWrapper ${./deploy-whitby.sh} $out/bin/deploy-whitby.sh \
      --prefix PATH : ${with pkgs; lib.makeBinPath [
        nix-diff
        ansi2html
        git
      ]}
  '';

  installCheckInputs = with pkgs; [
    shellcheck
  ];

  doInstallCheck = true;
  installCheckPhase = ''
    shellcheck $out/bin/deploy-whitby.sh
  '';
}
