{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage rec {
  pname = "notable";
  version = "1.0";
  src = ./notable.el;

  externalRequires = epkgs: with epkgs; [ f ht s ];

  internalRequires = [ depot.tools.emacs-pkgs.dottime ];
}
