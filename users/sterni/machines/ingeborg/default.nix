{ config, lib, pkgs, depot, ... }:

{
  imports = [
    # Basic settings
    ../../modules/common.nix
    # These modules touch things related to booting (filesystems, initrd network…)
    ./hardware.nix
    ./network.nix
  ];

  config = {
    system.stateVersion = "24.05";
  };
}
