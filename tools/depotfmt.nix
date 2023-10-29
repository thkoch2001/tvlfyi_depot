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
    command = "${depot.nix.buildGo.go}/bin/gofmt"
    options = [ "-w" ]
    includes = ["*.go"]

    [formatter.tf]
    command = "${terraformat}"
    includes = [ "*.tf" ]

    [formatter.nix]
    command = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
    includes = [ "*.nix" ]
    excludes = [
      "tvix/eval/src/tests/*",
    ]

    [formatter.rust]
    command = "${pkgs.rustfmt}/bin/rustfmt"
    includes = [ "*.rs" ]
    excludes = [
      "users/tazjin/*",
    ]
  '';

  # helper tool for formatting the depot interactively
  depotfmt = pkgs.writeShellScriptBin "depotfmt" ''
    exec ${pkgs.treefmt}/bin/treefmt ''${@} \
      --config-file ${config} \
      --tree-root $(${pkgs.git}/bin/git rev-parse --show-toplevel)
  '';

  # wrapper script for running formatting checks in CI
  check = pkgs.writeShellScript "depotfmt-check" ''
    ${pkgs.treefmt}/bin/treefmt \
      --clear-cache \
      --fail-on-change \
      --config-file ${config} \
      --tree-root .
  '';
in
depotfmt.overrideAttrs (_: {
  passthru.meta.ci.extraSteps.check = {
    label = "depot formatting check";
    command = check;
    alwaysRun = true;
  };
})
