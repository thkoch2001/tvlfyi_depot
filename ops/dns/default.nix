# Performs simple (local-only) validity checks on DNS zones.
{ pkgs, ... }:

let checkZone = zone: file: pkgs.runCommandNoCC "${zone}-check" {} ''
  ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
'';
in {
  tvl-fyi = checkZone "tvl.fyi" ./tvl.fyi.zone;
  meta.targets = [ "tvl-fyi" ];
}
