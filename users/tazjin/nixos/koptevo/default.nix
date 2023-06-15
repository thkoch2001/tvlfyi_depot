# NUC in my closet.
_: # ignore readTree options

{ config, depot, lib, pkgs, ... }:

let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
  usermod = name: depot.path.origSrc + ("/users/tazjin/nixos/modules/" + name);
in
{
  imports = [
    (mod "quassel.nix")
    (mod "www/base.nix")
    # (usermod "tgsa.nix")
    # (usermod "predlozhnik.nix")
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.initrd.availableKernelModules = [ "ata_piix" "vmw_pvscsi" "sd_mod" "sr_mod" ];


  nix.settings.trusted-users = [ "tazjin" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/TODO";
    fsType = "ext4";
  };

  networking = {
    hostName = "koptevo";
    domain = "tazj.in";
    useDHCP = true;
    firewall.enable = true;
  };

  time.timeZone = "UTC";

  security.acme.acceptTerms = true;
  security.acme.default.email = "acme@tazj.in";

  programs.fish.enable = true;

  users.users.tazjin = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
  };

  security.sudo.wheelNeedsPassword = false;

  services.depot.quassel = {
    enable = true;
    acmeHost = "koptevo.tazj.in";
    bindAddresses = [
      "0.0.0.0"
    ];
  };

  # Automatically collect garbage from the Nix store.
  services.depot.automatic-gc = {
    enable = true;
    interval = "daily";
    diskThreshold = 2; # GiB # TODO
    maxFreed = 8; # GiB
    preserveGenerations = "14d";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    curl
    htop
    jq
    nmap
    bat
    emacs-nox
    nano
    wget
  ];

  programs.mtr.enable = true;
  programs.mosh.enable = true;
  services.openssh.enable = true;
  system.stateVersion = "23.05";
}
