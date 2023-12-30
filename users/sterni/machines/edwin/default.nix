{ config, lib, pkgs, depot, ... }:

{
  imports = [
    # Third party modules we use
    "${depot.third_party.agenix.src}/modules/age.nix"
    # Basic settings
    ../../modules/common.nix
    # These modules touch things related to booting (filesystems, initrd network…)
    ./hardware.nix
    ./network.nix
    # These modules configure services, websites etc.
    (depot.path.origSrc + "/ops/modules/btrfs-auto-scrub.nix")
  ];

  config = {
    system.stateVersion = "20.09";
  };
}
