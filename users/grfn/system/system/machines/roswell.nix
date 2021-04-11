{ depot, config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../modules/common.nix
    "${modulesPath}/installer/scan/not-detected.nix"
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  ec2.hvm = true;

  networking.hostName = "roswell";

  users.users.grfn.openssh.authorizedKeys.keys = [
    depot.users.grfn.keys.main
  ];
}
