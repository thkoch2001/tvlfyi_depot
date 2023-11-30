{ config, lib, pkgs, depot, ... }:

{
  imports = [
    # Basic settings
    ../../modules/common.nix
    # These modules touch things related to booting (filesystems, initrd network…)
    ./hardware.nix
    ./network.nix
    # (More or less) pluggable service configuration
    (depot.path.origSrc + "/ops/modules/btrfs-auto-scrub.nix")
    ./monitoring.nix
  ];

  config = {
    system.stateVersion = "24.05";
  };
}
