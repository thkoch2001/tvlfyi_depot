{ depot, ... }:

depot.tools.emacs-pkgs.builder {
  pname = "tvl";
  version = "1.0";
  src = ./tvl.el;
  packageRequires = (epkgs: with epkgs; [ magit s ]);
}
