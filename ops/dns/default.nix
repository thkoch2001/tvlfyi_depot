# Performs simple (local-only) validity checks on DNS zones.
{ depot, pkgs, ... }:

let
  checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" { } ''
    ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
  '';

in
depot.nix.readTree.drvTargets {
  nixery-dev = checkZone "nixery.dev" ./nixery.dev.zone;
  tvl-fyi = checkZone "tvl.fyi" ./tvl.fyi.zone;
  tvl-su = checkZone "tvl.su" ./tvl.su.zone;
}
