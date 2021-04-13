{ depot, ... }:

depot.tools.emacs-pkgs.builder {
  pname = "term-switcher";
  version = "1.0";
  src = ./term-switcher.el;
  packageRequires = epkgs: with epkgs; [ dash ivy s vterm ];
}
