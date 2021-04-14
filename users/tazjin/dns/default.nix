# Performs simple (local-only) validity checks on DNS zones.
{ depot, pkgs, ... }:

let
  inherit (depot.nix.utils) drvTargets;

  checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" {} ''
    ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
  '';

in drvTargets {
  kontemplate-works = checkZone "kontemplate.works"./kontemplate.works.zone;
  tazj-in = checkZone "tazj.in" ./tazj.in.zone;
}
