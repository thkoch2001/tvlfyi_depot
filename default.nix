{ ... }:

let
  depot = import (builtins.fetchGit {
    url = "https://cl.tvl.fyi/depot";
    rev = "a2e86152401c7c531801c79347c3f15e1806aabc";
  }) {};
  readTree = depot.readTree {
    pkgs = import (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      rev = "f1a79c86358c5464c64b4fad00fca07a10e62a74";
    }) {};
    unstable = import (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      rev = "b3251e04ee470c20f81e75d5a6080ba92dc7ed3f";
    }) {};
    briefcase = import (builtins.path {
      path = ./.;
      name = "briefcase";
    }) {};
    depot = depot;
  };
in {
  nixos       = readTree ./nixos;
  utils       = readTree ./utils;
  emacs       = readTree ./emacs;
  website     = readTree ./website;
  lisp        = readTree ./lisp;
  gopkgs      = readTree ./gopkgs;
  monzo_ynab  = readTree ./monzo_ynab;
  third_party = readTree ./third_party;
  tools       = readTree ./tools;
}
