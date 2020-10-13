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

  time.timeZone = "America/New_York";

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
    depot.users.glittershark.system.system.rebuilder
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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;

  nix = {
    trustedUsers = [ "grfn" ];
    autoOptimiseStore = true;

    buildMachines = [{
      hostName = "172.16.0.3";
      sshUser = "griffin";
      sshKey = "/home/grfn/.ssh/id_rsa";
      system = "x86_64-darwin";
      maxJobs = 4;
    } {
      hostName = "172.16.0.4";
      sshUser = "griffin";
      sshKey = "/home/grfn/.ssh/id_rsa";
      system = "x86_64-darwin";
      maxJobs = 8; # 16 cpus
    } {
      hostName = "eu.nixbuild.net";
      system = "x86_64-linux";
      maxJobs = 100;
      supportedFeatures = [ "benchmark" "big-parallel" ];
    }];

    distributedBuilds = true;

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
}
