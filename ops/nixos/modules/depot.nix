# This module makes it possible to get at the depot from "proper"
# NixOS modules.
#
# It needs to be included and configured in each system like this:
#
# {
#   imports = [ ../modules/depot.nix ];
#   inherit depot;
# }
{ lib, ... }:

{
  options.depot = with lib; mkOption {
    description = "tazjin's imported monorepo";
  };
}
