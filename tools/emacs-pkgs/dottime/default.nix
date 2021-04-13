{ depot, ... }:

depot.tools.emacs-pkgs.builder {
  pname = "dottime";
  version = "1.0";
  src = ./dottime.el;
}
