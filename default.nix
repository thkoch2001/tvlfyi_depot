{ ... }:

let
  depot = import (builtins.fetchGit {
    url = "https://cl.tvl.fyi/depot";
    rev = "a2e86152401c7c531801c79347c3f15e1806aabc";
  }) {};
  readTree = depot.nix.readTree {
    pkgs = import (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs-channels";
      ref = "nixos-20.03";
      rev = "afa9ca61924f05aacfe495a7ad0fd84709d236cc";
    }) {};
    briefcase = import (builtins.path {
      path = ./.;
      name = "briefcase";
    }) {};
    depot = depot;
  };
in {
  ci           = readTree ./ci;
  nixos        = readTree ./nixos;
  utils        = readTree ./utils;
  emacs        = readTree ./emacs;
  website      = readTree ./website;
  lisp         = readTree ./lisp;
  gopkgs       = readTree ./gopkgs;
  third_party  = readTree ./third_party;
  tools        = readTree ./tools;
  buildHaskell = readTree ./buildHaskell;
  zoo          = readTree ./zoo;
}
