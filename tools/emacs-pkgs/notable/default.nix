{ pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "notable";
  version = "1.0";
  src = ./notable.el;
  packageRequires = with pkgs.emacsPackages; [
    dash f ht s
  ];
}
