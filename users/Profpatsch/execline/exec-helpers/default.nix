{ depot, ... }:
depot.nix.writers.rustSimpleLib
{
  name = "exec-helpers";
}
  (builtins.readFile ./exec_helpers.rs)
