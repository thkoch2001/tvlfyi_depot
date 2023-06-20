{ depot, lib, ... }:

let systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in depot.nix.readTree.drvTargets {
  camdenSystem = systemFor depot.users.tazjin.nixos.camden;
  frogSystem = systemFor depot.users.tazjin.nixos.frog;
  tverskoySystem = systemFor depot.users.tazjin.nixos.tverskoy;
  zamalekSystem = systemFor depot.users.tazjin.nixos.zamalek;
  koptevoSystem = systemFor depot.users.tazjin.nixos.koptevo;
}
