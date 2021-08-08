# Temporary fix for unstable telega that tries to build with a broken patch.
{ pkgs, ... }:

pkgs.emacsPackages.melpaPackages.telega.overrideAttrs(old: {
  patches = [];
})
