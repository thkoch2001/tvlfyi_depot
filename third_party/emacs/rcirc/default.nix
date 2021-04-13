{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage rec {
  pname = "rcirc";
  version = "1";
  src = ./rcirc.el;
}
