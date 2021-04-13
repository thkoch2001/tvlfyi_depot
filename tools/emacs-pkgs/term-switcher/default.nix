{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "term-switcher";
  version = "1.0";
  src = ./term-switcher.el;
  externalRequires = epkgs: with epkgs; [ dash ivy s vterm ];
}
