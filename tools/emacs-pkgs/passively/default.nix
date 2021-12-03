{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "passively";
  version = "1.0";
  src = ./passively.el;
  externalRequires = (epkgs: with epkgs; [ ht ]);
}
