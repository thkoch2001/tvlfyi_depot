{ pkgs, depot, ... }:

pkgs.callPackage
  ({ emacsPackages }:
  emacsPackages.trivialBuild {
    pname = "tuple";
    version = "1.0.0";
    src = ./tuple.el;
  })
{ }
