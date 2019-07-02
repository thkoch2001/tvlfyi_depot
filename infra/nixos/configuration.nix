# This file contains basic configuration for all *.tazj.in Nix machines.

{ config, pkgs, ... }:

{
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  boot.initrd.availableKernelModules = [
    "ata_piix"
    "mptspi"
    "sd_mod"
    "sr_mod"
  ];

  # Configure root disk
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  services.vmwareGuest.enable = true;
  services.vmwareGuest.headless = true;

  time.timeZone = "Europe/Oslo";

  environment.systemPackages = with pkgs; [
    curl emacs htop
  ];

  services.openssh.enable = true;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];

  users.extraUsers.vincent = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
  };

  security.sudo = {
    enable = true;
    extraConfig = "%wheel ALL=(ALL) NOPASSWD: ALL";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "17.09"; # Did you read the comment?
}
