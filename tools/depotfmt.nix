# Builds treefmt for depot, with a hardcoded configuration that
# includes the right paths to formatters.
{ depot, pkgs, ... }:

let
  config = pkgs.writeText "depot-treefmt-config" ''
    [formatter.go]
    command = "${pkgs.go}/bin/gofmt"
    options = [ "-w" ]
    includes = ["*.go"]
  '';

  # helper tool for formatting the depot interactively
  depotfmt = pkgs.writeShellScriptBin "depotfmt" ''
    exec ${pkgs.treefmt}/bin/treefmt ''${@} \
      --config-file ${config} \
      --tree-root $(${pkgs.git}/bin/git rev-parse --show-toplevel)
  '';

  # wrapper for failure in CI
  check = pkgs.runCommandNoCC "depotfmt-check" {} ''
    cp -r ${depot.path.origSrc} depot
    exec ${pkgs.treefmt}/bin/treefmt --fail-on-change \
      --config-file ${config} \
      --tree-root depot
  '';
in depotfmt // depot.nix.readTree.drvTargets { inherit check; }
