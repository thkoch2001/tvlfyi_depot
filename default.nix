{ ... }:

let
  inherit (builtins) fetchGit readDir path;
  inherit (pkgs.lib) filterAttrs mapAttrs;
  inherit (pkgs.lib.strings) hasPrefix;

  briefcasePath = path {
    path = ./.;
    name = "briefcase";
  };

  depot = import (fetchGit {
    url = "https://cl.tvl.fyi/depot";
    rev = "2f7b688389058b454ee12adc4b6b47740298f53b";
  }) {};

  pkgs = import (fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "nixos-20.03";
    rev = "afa9ca61924f05aacfe495a7ad0fd84709d236cc";
  }) {};

  briefcase = import briefcasePath {};

  readTree = depot.nix.readTree {
    inherit depot pkgs briefcase;
  };
in mapAttrs
  (name: _: readTree (./. + "/${name}"))
  (filterAttrs
    (name: type: type == "directory" && !hasPrefix "." name)
    (readDir briefcasePath))
