{ config, lib, pkgs, ... }:

let

  depot = import ../../../../.. {};

in

with lib;

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.networkmanager.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = lib.mkDefault "America/New_York";

  environment.systemPackages = with pkgs; [
    wget
    vim
    zsh
    git
    w3m
    libnotify
    file
    lm_sensors
    dnsutils
    htop
  ];

  services.openssh.enable = true;

  programs.ssh.startAgent = true;

  networking.firewall.enable = mkDefault false;

  users.mutableUsers = true;
  programs.zsh.enable = true;
  environment.pathsToLink = [ "/share/zsh" ];
  users.users.grfn = {
    isNormalUser = true;
    initialPassword = "password";
    extraGroups = [
      "wheel"
      "networkmanager"
      "audio"
      "docker"
    ];
    shell = pkgs.zsh;
  };

  nix = {
    trustedUsers = [ "grfn" ];
    autoOptimiseStore = true;
    distributedBuilds = true;

    gc = {
      automatic = true;
      dates = mkDefault "weekly";
      options = "--delete-older-than 30d";
    };
  };

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

  services.pcscd.enable = true;
}
