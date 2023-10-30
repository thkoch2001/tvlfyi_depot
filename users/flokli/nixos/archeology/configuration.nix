{ depot, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../profiles/archeology.nix
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";

  boot.kernelParams = [ "console=ttyS0" ];

  networking.hostName = "archeology";

  system.stateVersion = "23.05"; # Did you read the comment?
}

