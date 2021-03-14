# Performs simple (local-only) validity checks on DNS zones.
{ pkgs, ... }:

let
  checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" {} ''
    ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
  '';

  zones = {
    tvl-fyi = checkZone "tvl.fyi" ./tvl.fyi.zone;
    tvl-su = checkZone "tvl.su" ./tvl.su.zone;
  };
in zones // {
  meta.targets = builtins.attrNames zones;
}
