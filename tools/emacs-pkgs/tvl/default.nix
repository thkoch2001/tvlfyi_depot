{ depot, ... }:

depot.tools.emacs-pkgs.builder {
  pname = "tvl";
  version = "1.0";
  src = ./tvl.el;
  externalRequires = (epkgs: with epkgs; [ magit s ]);
}
