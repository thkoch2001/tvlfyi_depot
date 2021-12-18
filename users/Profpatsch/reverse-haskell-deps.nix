{ depot, pkgs, ... }:

# Parses https://packdeps.haskellers.com/reverse
# and outputs the amount of reverse dependencies of each hackage package.

let

  rev = depot.nix.writeExecline "reverse-haskell-deps" { } [
    "pipeline"
    [
      "${pkgs.curl}/bin/curl"
      "-L"
      "https://packdeps.haskellers.com/reverse"
    ]
    rev-hs

  ];

  rev-hs = pkgs.writers.writeHaskell "revers-haskell-deps-hs"
    {
      libraries = [
        pkgs.haskellPackages.nicify-lib
        pkgs.haskellPackages.tagsoup
      ];

    }
    ./reverse-haskell-deps.hs;


in
rev
