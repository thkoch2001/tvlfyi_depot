# Check protobuf syntax and breaking.
#
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "ci-buf-check" ''
  ${depot.third_party.nixpkgs.buf}/bin/buf lint .
  # Report-only
  ${depot.third_party.nixpkgs.buf}/bin/buf breaking . --against "./.git#ref=HEAD~1" || true
''
