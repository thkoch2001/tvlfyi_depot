{ depot, lib, ... }:

let systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in {
  marcusSystem = systemFor depot.users.wpcarro.nixos.marcus;
  meta.targets = [ "marcusSystem" ];
}
