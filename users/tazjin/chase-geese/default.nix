# Helpers for mounting GeeseFS into the right place.
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "chase-geese" ''
  set -ueo pipefail

  echo "Fetching credentials ..."
  eval $(pass show keys/tazjin-geesefs)

  echo "Mounting the cloud ..."
  mkdir -p ~/cloud
  ${pkgs.geesefs}/bin/geesefs tazjins-files ~/cloud
''
