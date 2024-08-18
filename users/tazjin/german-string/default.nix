{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
}
