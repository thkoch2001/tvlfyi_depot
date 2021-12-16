{ depot, lib, ... }:

let systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in {
  diogenesSystem = systemFor depot.users.wpcarro.nixos.diogenes;
  marcusSystem = systemFor depot.users.wpcarro.nixos.marcus;

  meta.targets = [ "diogenesSystem" "marcusSystem" ];
}
