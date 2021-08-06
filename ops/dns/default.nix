# Performs simple (local-only) validity checks on DNS zones.
{ depot, pkgs, ... }:

let
  inherit (depot.nix.utils)
    drvTargets
    ;

  checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" {} ''
    ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
  '';

in drvTargets {
  nixery-dev = checkZone "nixery.dev" ./nixery.dev.zone;
  tvl-fyi = checkZone "tvl.fyi" ./tvl.fyi.zone;
  tvl-su = checkZone "tvl.su" ./tvl.su.zone;
}
