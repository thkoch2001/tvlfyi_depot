{ pkgs, ... }:

pkgs.callPackage
  ({ emacsPackages }:
  emacsPackages.trivialBuild {
    pname = "zle";
    version = "1.0.0";
    src = ./zle.el;
  })
{ }
