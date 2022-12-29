{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.zathura [ "zathura" ]
    // depot.nix.getBins pkgs.haskellPackages.graphmod [ "graphmod" ]
    // depot.nix.getBins pkgs.graphviz [ "dot" ]
  ;

  # Display a graph of all modules in a project and how they depend on each other.
  # Takes the project directory as argument.
  # Open in zathura.
  haskell-module-deps = depot.nix.writeExecline "haskell-module-deps" { } [
    "pipeline"
    [ haskell-module-deps-with-filetype "pdf" "$@" ]
    bins.zathura
    "-"
  ];

  # Display a graph of all modules in a project and how they depend on each other.
  # Takes the project directory as argument.
  # Print a png to stdout.
  haskell-module-deps-png = depot.nix.writeExecline "haskell-module-deps-png" { } [
    haskell-module-deps-with-filetype
    "png"
    "$@"
  ];

  # Display a graph of all modules in a project and how they depend on each other.
  # Takes the file type to generate as first argument
  # and the project directory as second argument.
  haskell-module-deps-with-filetype = pkgs.writers.writeBash "haskell-module-deps-with-filetype" ''
    set -euo pipefail
    shopt -s globstar
    filetype="$1"
    rootDir="$2"
    ${bins.graphmod} \
      ${/*silence warnings for missing external dependencies*/""} \
      --quiet \
      ${/*applies some kind of import simplification*/""} \
      --prune-edges \
      "$rootDir"/src/**/*.hs \
      | ${bins.dot} \
          ${/*otherwise it’s a bit cramped*/""} \
          -Gsize="20,20!" \
          -T"$filetype"
  '';

in
depot.nix.readTree.drvTargets {
  inherit
    haskell-module-deps
    haskell-module-deps-png
    haskell-module-deps-with-filetype
    ;
}
