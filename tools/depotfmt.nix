# Builds treefmt for depot, with a hardcoded configuration that
# includes the right paths to formatters.
{ depot, pkgs, ... }:

let
  # terraform fmt can't handle multiple paths at once, but treefmt
  # expects this
  terraformat = pkgs.writeShellScript "terraformat" ''
    echo "$@" | xargs -n1 ${pkgs.terraform}/bin/terraform fmt
  '';

  config = pkgs.writeText "depot-treefmt-config" ''
    [formatter.go]
    command = "${pkgs.go}/bin/gofmt"
    options = [ "-w" ]
    includes = ["*.go"]

    [formatter.tf]
    command = "${terraformat}"
    includes = [ "*.tf" ]
  '';

  # helper tool for formatting the depot interactively
  depotfmt = pkgs.writeShellScriptBin "depotfmt" ''
    exec ${pkgs.treefmt}/bin/treefmt ''${@} \
      --config-file ${config} \
      --tree-root $(${pkgs.git}/bin/git rev-parse --show-toplevel)
  '';

  # wrapper for running formatting checks in CI
  check = pkgs.runCommandNoCC "depotfmt-check" {} ''
    ${pkgs.git}/bin/git clone ${depot.path.origSrc} depot
    export HOME="$(${pkgs.coreutils}/bin/realpath .)"
    ${pkgs.treefmt}/bin/treefmt \
      --fail-on-change \
      --config-file ${config} \
      --tree-root depot && : > $out
  '';
in depotfmt // depot.nix.readTree.drvTargets { inherit check; }
