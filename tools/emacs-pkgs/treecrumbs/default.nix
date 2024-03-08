{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "treecrumbs";
  version = "1.0";
  src = ./treecrumbs.el;
}
