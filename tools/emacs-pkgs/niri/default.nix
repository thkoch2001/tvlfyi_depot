{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage rec {
  pname = "niri";
  version = "1.0";
  src = ./niri.el;
}
