{ pkgs ? (import ../../../. {}).third_party, ... }:

pkgs.haskellPackages.callCabal2nix "owothia"
  (pkgs.gitignoreSource ./.) { }
