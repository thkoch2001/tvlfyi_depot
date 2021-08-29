# Check protobuf syntax and breaking.
#
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "ci-buf-check" ''
  ${depot.third_party.bufbuild}/bin/buf check lint --input .
  # Report-only
  ${depot.third_party.bufbuild}/bin/buf check breaking --input "." --against-input "./.git#branch=canon" || true
''
