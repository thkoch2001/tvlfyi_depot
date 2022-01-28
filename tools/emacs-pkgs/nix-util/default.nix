{ depot
, ...
}:
depot.tools.emacs-pkgs.buildEmacsPackage
  { pname = "nix-util"; version = "1.0"; src = ./nix-util.el; externalRequires = epkgs: [ epkgs.s ]; }
