{ pkgs, depot, ... }:

pkgs.callPackage
  ({ emacsPackages }:
  emacsPackages.trivialBuild {
    pname = "bookmark";
    version = "1.0.0";
    src = ./bookmark.el;
    packageRequires = (with pkgs.emacsPackages; [
      general
    ]);
  })
{ }
