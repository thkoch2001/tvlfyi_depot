# Utility script for building any arbitrary depot path in its folder.
{ pkgs, ... }:

pkgs.writeShellScriptBin "depot-build" ''
  TARGET=$(git rev-parse --show-prefix | sed 's|/$||')
  echo "Building //$TARGET"
  nix-build -A $(echo $TARGET | sed 's|/|.|g') $(${pkgs.git}/bin/git rev-parse --show-toplevel)
''
