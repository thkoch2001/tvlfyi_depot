# Check protobuf syntax and breaking.
#
{ depot, pkgs, ... }:

pkgs.runCommand "ci-buf-check" {} ''
    ${depot.third_party.bufbuild}/bin/buf check lint --input "${depot.depotPath}"
    ${depot.third_party.bufbuild}/bin/buf check breaking --input "${depot.depotPath}" --against-input "${depot.depotPath}/.git#branch=canon"
  ''
