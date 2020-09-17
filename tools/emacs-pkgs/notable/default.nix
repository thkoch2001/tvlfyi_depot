{ depot, pkgs, ... }:

pkgs.emacsPackages.trivialBuild rec {
  pname = "notable";
  version = "1.0";
  src = ./notable.el;
  packageRequires = with pkgs.emacsPackages; [
    f ht s
    depot.tools.emacs-pkgs.dottime
  ];
}
