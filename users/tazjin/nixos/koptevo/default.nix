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

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.availableKernelModules = [ "ahci" "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
    kernelModules = [ "kvm-intel" ];
  };

  nix.settings.trusted-users = [ "tazjin" ];

  fileSystems = {
    "/" = {
      device = "rpool/root";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/5684-3C1A";
      fsType = "vfat";
    };

    "/var" = {
      device = "rpool/var";
      fsType = "zfs";
    };

    "/home" = {
      device = "rpool/home";
      fsType = "zfs";
    };
  };

  hardware.cpu.intel.updateMicrocode = true;
  services.fwupd.enable = true;

  networking = {
    hostName = "koptevo";
    hostId = "07bbbf4f";
    domain = "tazj.in";
    useDHCP = true;
    firewall.enable = true;

    wireless.enable = true;
    wireless.networks."How do I computer fast?" = {
      psk = "washyourface";
    };
  };

  time.timeZone = "UTC";

  security.acme.acceptTerms = true;
  security.acme.defaults.email = "acme@tazj.in";

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
