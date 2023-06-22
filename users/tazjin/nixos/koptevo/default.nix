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
    (mod "www/tazj.in.nix")
    (usermod "monica.nix")
    (usermod "predlozhnik.nix")
    (usermod "tgsa.nix")
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.availableKernelModules = [ "ahci" "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "nomodeset" ];
  };

  nix.settings.trusted-users = [ "tazjin" ];

  fileSystems = {
    "/" = {
      device = "rpool/root";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/E214-E6B3";
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
  hardware.enableRedistributableFirmware = true;
  services.fwupd.enable = true;

  networking = {
    hostName = "koptevo";
    hostId = "07bbbf4f";
    domain = "tazj.in";
    useDHCP = true;
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22 80 443 ];

    wireless.enable = true;
    wireless.networks."How do I computer fast?" = {
      psk = "washyourface";
    };
  };

  time.timeZone = "UTC";

  security.acme.acceptTerms = true;
  security.acme.defaults.email = lib.mkForce "acme@tazj.in";

  programs.fish.enable = true;

  users.users.tazjin = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "systemd-journal" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
  };

  age.secrets =
    let
      secretFile = name: depot.users.tazjin.secrets."${name}.age";
    in
    {
      tgsa-yandex.file = secretFile "tgsa-yandex";
    };

  security.sudo.wheelNeedsPassword = false;

  services.openssh.enable = true;

  services.depot.quassel = {
    enable = true;
    acmeHost = "koptevo.tazj.in";
    bindAddresses = [
      "0.0.0.0"
    ];
  };

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server"; # for exit-node usage
  };

  # Automatically collect garbage from the Nix store.
  services.depot.automatic-gc = {
    enable = true;
    interval = "daily";
    diskThreshold = 2; # GiB # TODO
    maxFreed = 8; # GiB
    preserveGenerations = "14d";
  };

  services.nginx.virtualHosts."koptevo.tazj.in" = {
    addSSL = true;
    enableACME = true;

    extraConfig = ''
      location = / {
        return 302 https://at.tvl.fyi/?q=%2F%2Fusers%2Ftazjin%2Fnixos%2Fkoptevo%2Fdefault.nix;
      }
    '';
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

  system.stateVersion = "23.05";
}
