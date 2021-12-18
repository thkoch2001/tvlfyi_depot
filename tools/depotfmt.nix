# Builds treefmt for depot, with a hardcoded configuration that
# includes the right paths to formatters.
{ pkgs, ... }:

let
  config = pkgs.writeText "depot-treefmt-config" ''
    [formatter.go]
    command = "${pkgs.go}/bin/gofmt"
    options = [ "-w" ]
    includes = ["*.go"]
  '';
in pkgs.writeShellScriptBin "depotfmt" ''
  exec ${pkgs.treefmt}/bin/treefmt ''${@} \
    --config-file ${config} \
    --tree-root $(${pkgs.git}/bin/git rev-parse --show-toplevel)
''
