{ pkgs, ... }:

pkgs.writeShellScriptBin "depot-scan" ''
  set -euo pipefail

  path=$(git rev-parse --show-prefix)
  path="''${path%%/}"
  attr="''${path//\//.}"
  root="$(git rev-parse --show-toplevel)"
  echo "scanning //$path" >&2
  nix-instantiate "$root" -A "$attr" -vv 2> >(${pkgs.perl}/bin/perl sources.pl) >&2
''
