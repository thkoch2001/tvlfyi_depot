{ pkgs, depot, ... }:

let
  commonsh = ./common.sh;

  # TODO(riking): path deduction
  #tvix-instantiate="${third_party.nix}/bin/nix-instantiate"
  pathScripts = pkgs.writeShellScript "imports" ''
  '';

  # setup: git rev-parse --show-toplevel > $tvlc_root/depot_root
  # setup: mkdir $tvlc_root/clients
  # setup: echo 1 > $tvlc_root/next_clientid

  tvlcNew = pkgs.stdenv.mkDerivation {
    name = "tvlc-new";
    src = ./tvlc-new;
    doCheck = true;

    unpackPhase = "true";
    buildPhase = ''
      substitute ${./tvlc-new} $out --replace common.sh ${commonsh}
    '';
    checkPhase = ''
      ${pkgs.shellcheck}/bin/shellcheck $out ${commonsh} && echo "SHELLCHECK OK"
    '';
    installPhase = ''
      chmod +x $out
    '';
  };

in pkgs.stdenv.mkDerivation rec {
  inherit commonsh;
  inherit tvlcNew;
}
