{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "deploy-whitby";

  phases = [ "installPhase" "installCheckPhase" ];

  nativeBuildInputs = with pkgs; [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${./deploy-whitby.sh} $out/bin/deploy-whitby.sh \
      --prefix PATH : ${
        with pkgs;
        lib.makeBinPath [ ansi2html git jq nix-diff ]
      }
  '';

  installCheckInputs = with pkgs; [ shellcheck ];

  doInstallCheck = true;
  installCheckPhase = ''
    shellcheck $out/bin/deploy-whitby.sh
  '';
}
