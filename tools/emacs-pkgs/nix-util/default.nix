{ depot, ... }:

depot.tools.emacs-pkgs.builder {
  pname = "nix-util";
  version = "1.0";
  src = ./nix-util.el;
}
