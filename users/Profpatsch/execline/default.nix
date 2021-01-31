{ depot, pkgs, lib, ... }:

let
  exec-helpers = depot.users.Profpatsch.writers.rustSimpleLib {
    name = "exec-helpers";
  } (builtins.readFile ./exec_helpers.rs);

in {
  inherit
    exec-helpers
    ;
}
