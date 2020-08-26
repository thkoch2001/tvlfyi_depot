{ pkgs, depot, ... }:

let
  pathScripts = pkgs.writeShellScript "imports" ''
    export tvix_instantiate="${depot.third_party.nix}/bin/nix-instantiate"
    export depot_scanner="${depot.tools.depot-scanner}/bin/depot-scanner"
  '';

  # setup: git rev-parse --show-toplevel > $tvlc_root/depot_root
  # setup: mkdir $tvlc_root/clients
  # setup: echo 1 > $tvlc_root/next_clientid

  commonsh = pkgs.stdenv.mkDerivation {
    name = "common.sh";
    src = ./common.sh;
    doCheck = true;
    unpackPhase = "true";
    buildPhase = ''
      substitute ${./common.sh} $out --replace path-scripts ${pathScripts}
    '';
    checkPhase = ''
      ${pkgs.shellcheck}/bin/shellcheck $out ${pathScripts} && echo "SHELLCHECK OK"
    '';
    installPhase = ''
      chmod +x $out
    '';
  };

  tvlcNew = pkgs.stdenv.mkDerivation {
    name = "tvlc-new";
    src = ./tvlc-new;
    doCheck = true;

    unpackPhase = "true";
    buildPhase = ''
      substitute ${./tvlc-new} $out --replace common.sh ${commonsh}
    '';
    checkPhase = ''
      ${pkgs.shellcheck}/bin/shellcheck $out ${commonsh} ${pathScripts} && echo "SHELLCHECK OK"
    '';
    installPhase = ''
      chmod +x $out
    '';
  };

in {
  inherit pathScripts;
  inherit commonsh;
  inherit tvlcNew;
}
