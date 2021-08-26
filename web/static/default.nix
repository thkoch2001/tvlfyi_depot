{ pkgs, ... }:

# Needs to be a derivation ...
pkgs.runCommand "tvl-static" {} ''
  cp -r ${./.} $out
''
