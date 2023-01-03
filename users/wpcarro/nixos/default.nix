{ depot, pkgs, ... }:

let
  inherit (depot.users.wpcarro.nixos)
    ava
    kyoko
    marcus
    tarasco;

  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in
{
  avaSystem = systemFor ava;
  kyokoSystem = systemFor kyoko;
  marcusSystem = systemFor marcus;
  tarascoSystem = systemFor ava;

  meta.ci.targets = [
    "avaSystem"
    "kyokoSystem"
    "marcusSystem"
    "tarascoSystem"
  ];
}
