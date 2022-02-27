{ depot, pkgs, ... }:

pkgs.callPackage depot.third_party.sources.napalm { } // {
  meta.ci.targets = [
    "napalm-registry"
  ];
}
