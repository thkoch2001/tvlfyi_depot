{ depot, lib, ... }:

let systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in
{
  camdenSystem = systemFor depot.users.tazjin.nixos.camden;
  frogSystem = systemFor depot.users.tazjin.nixos.frog;
  tverskoySystem = systemFor depot.users.tazjin.nixos.tverskoy;

  meta.targets = [ "camdenSystem" "frogSystem" "tverskoySystem" ];
}
