{ pkgs, depot, ... }:

pkgs.callPackage
  ({ emacsPackages }:
    emacsPackages.trivialBuild {
      pname = "macros";
      version = "1.0.0";
      src = ./macros.el;
    })
  { }
