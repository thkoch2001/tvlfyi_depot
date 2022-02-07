# Builds treefmt for depot, with a hardcoded configuration that
# includes the right paths to formatters.
{ depot, pkgs, ... }:

let
  # terraform fmt can't handle multiple paths at once, but treefmt
  # expects this
  terraformat = pkgs.writeShellScript "terraformat" ''
    echo "$@" | xargs -n1 ${pkgs.terraform}/bin/terraform fmt
  '';

  rustfmtConfig = pkgs.writeTextDir "rustfmt.toml" ''
    edition = "2018"
    newline_style = "Unix"

    # Default code with is 100 characters, comments should follow
    # suit.
    wrap_comments = true
    comment_width = 100

    # The default of this option creates hard-to-read nesting of
    # conditionals, turn it off.
    combine_control_expr = false

    # Group imports by module, but no higher. This avoids hard-to-read
    # nested use statements.
    imports_granularity = "Module"

    # Avoid vertical visual clutter by unnecessarily exploding
    # block-like arguments.
    overflow_delimited_expr = true

    # Miscellaneous
    format_code_in_doc_comments = true
    match_block_trailing_comma = true
    normalize_comments = true
  '';

  config = pkgs.writeText "depot-treefmt-config" ''
    [formatter.go]
    command = "${pkgs.go}/bin/gofmt"
    options = [ "-w" ]
    includes = ["*.go"]

    [formatter.tf]
    command = "${terraformat}"
    includes = [ "*.tf" ]

    [formatter.nix]
    command = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
    includes = [ "*.nix" ]
    excludes = [
      "third_party/nix/tests/*",
      "third_party/nix/src/tests/*"
    ]

    [formatter.rust]
    command = "${pkgs.rustfmt}/bin/rustfmt"
    includes = [ "*.rs" ]
    options = [
      "--config-path", "${rustfmtConfig}"
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
