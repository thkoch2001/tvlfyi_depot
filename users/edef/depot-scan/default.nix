{ pkgs, ... }:

pkgs.writeShellScriptBin "depot-scan" ''
  set -euo pipefail

  path="''${1:-$(git rev-parse --show-prefix)}"
  path="''${path%%/}"
  attr="''${path//\//.}"
  root="$(git rev-parse --show-toplevel)"
  echo "scanning //$path" >&2
  nix-instantiate -E "import ${./wrap.nix} $root" -A "$attr" --trace-file-access 2> >(${pkgs.perl}/bin/perl ${./depot-scan.pl}) >&2
''
