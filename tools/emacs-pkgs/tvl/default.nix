{ pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "tvl";
  version = "1.0";
  src = ./tvl.el;
}
