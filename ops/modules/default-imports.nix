{ depot, ... }:

# Default set of modules that are imported in all Depot nixos systems
#
# All modules here should be properly gated behind a `lib.mkEnableOption` with a
# `lib.mkIf` for the config.

{
  imports = [
    "${depot.path + "/ops/modules/automatic-gc.nix"}"
    "${depot.path + "/ops/modules/tvl-cache.nix"}"
  ];
}
