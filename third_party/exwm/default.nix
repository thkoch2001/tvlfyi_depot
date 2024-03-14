{
  depot,
  pkgs,
  lib,
  ...
}:

# special dance for overriding this into the fixed-point of emacs
# packages, but having it be separately buildable.

pkgs.emacsPackages.callPackage (
  { trivialBuild, xelb }:
  trivialBuild {
    pname = "depot-exwm";
    version = "canon";
    src = ./.;

    packageRequires = [ xelb ];
  }
) { }
