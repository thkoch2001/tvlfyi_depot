# Performs simple (local-only) validity checks on DNS zones.
{ depot, pkgs, ... }:

let
  checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" { } ''
    ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
  '';

in
depot.nix.readTree.drvTargets {
  kontemplate-works = checkZone "kontemplate.works" ./kontemplate.works.zone;
  tazj-in = checkZone "tazj.in" ./tazj.in.zone;
}
