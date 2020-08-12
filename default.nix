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
    unstable = import (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs-channels";
      ref = "nixos-unstable";
      rev = "dc80d7bc4a244120b3d766746c41c0d9c5f81dfa";
    }) {};
    briefcase = import (builtins.path {
      path = ./.;
      name = "briefcase";
    }) {};
    depot = depot;
  };
in {
  nixos        = readTree ./nixos;
  utils        = readTree ./utils;
  emacs        = readTree ./emacs;
  website      = readTree ./website;
  lisp         = readTree ./lisp;
  gopkgs       = readTree ./gopkgs;
  monzo_ynab   = readTree ./monzo_ynab;
  third_party  = readTree ./third_party;
  tools        = readTree ./tools;
  buildHaskell = readTree ./buildHaskell;
}
