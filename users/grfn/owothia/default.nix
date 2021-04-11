{ depot ? (import ../../../. {})
, pkgs ? depot.third_party.nixpkgs
, ... }:

pkgs.haskellPackages.callCabal2nix "owothia"
  (depot.third_party.gitignoreSource ./.) { }
