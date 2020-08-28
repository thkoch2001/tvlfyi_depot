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
    rev = "a2e86152401c7c531801c79347c3f15e1806aabc";
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
