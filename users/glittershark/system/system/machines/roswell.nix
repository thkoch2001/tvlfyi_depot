{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../modules/common.nix
    "${modulesPath}/installer/scan/not-detected.nix"
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  ec2.hvm = true;

  networking.hostName = "roswell";

  users.users.grfn.openssh.authorizedKeys.keys = [
    config.depot.users.glittershark.keys.main
  ];

  home-manager.users.grfn = { ... }: {
    imports = [ ../../home/machines/roswell.nix ];
    lib.depot = config.depot;
  };
}
