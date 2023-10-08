# Base image for Yandex Cloud VMs.
{ depot, ... }:

(depot.ops.nixos.nixosFor {
  imports = [
    (depot.path.origSrc + ("/ops/modules/yandex-cloud.nix"))
    (depot.path.origSrc + ("/ops/modules/tvl-users.nix"))
  ];
}).config.system.build.yandexCloudImage
