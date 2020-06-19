{ pkgs, ... }:

with pkgs.emacsPackages;

trivialBuild rec {
  pname = "tvl";
  version = "1.0";
  src = ./tvl.el;
  packageRequires = [ magit ];
}
