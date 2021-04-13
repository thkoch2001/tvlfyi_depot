{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "dottime";
  version = "1.0";
  src = ./dottime.el;
}
