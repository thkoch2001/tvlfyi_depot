{ depot, pkgs, ... }:

{ name, src, deps ? (_: [ ]), emacs ? pkgs.emacs27-nox }:

let
  inherit (pkgs) emacsPackages emacsPackagesGen;
  inherit (builtins) isString toFile;

  finalEmacs = (emacsPackagesGen emacs).emacsWithPackages deps;

  srcFile =
    if isString src
    then toFile "${name}.el" src
    else src;

in
depot.nix.writeScriptBin name ''
  #!/bin/sh
  ${finalEmacs}/bin/emacs --batch --no-site-file --script ${srcFile} $@
''
