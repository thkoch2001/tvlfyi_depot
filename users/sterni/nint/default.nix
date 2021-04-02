{ depot, pkgs, ... }:

let
  inherit (depot.users.Profpatsch.writers)
    rustSimpleBin
    ;
in

  rustSimpleBin {
    name = "nint";
    dependencies = [
      depot.third_party.rust.serde_json
    ];
  } (builtins.readFile ./nint.rs)
