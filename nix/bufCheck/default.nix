# Check protobuf syntax and breaking.
#
{ depot, pkgs, ... }:

pkgs.writeShellScriptBin "ci-buf-check" ''
  ${depot.third_party.bufbuild}/bin/buf check lint --input "${depot.path}"
  # Report-only
  ${depot.third_party.bufbuild}/bin/buf check breaking --input "${depot.path}" --against-input "${depot.path}/.git#branch=canon" || true
''
