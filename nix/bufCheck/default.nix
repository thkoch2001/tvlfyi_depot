# Check protobuf breaking. Lints already happen in individual targets.
#
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "ci-buf-check" ''
  export PATH="$PATH:${pkgs.lib.makeBinPath [ pkgs.buf ]}"
  # Report-only
  (cd $(git rev-parse --show-toplevel) && (buf breaking . --against "./.git#ref=HEAD~1" || true))
''
