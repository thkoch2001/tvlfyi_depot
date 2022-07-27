# VPS hosted at GleSYS, running my Quassel and some random network
# stuff.

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
    (usermod "tgsa.nix")
    (usermod "predlozhnik.nix")
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
  boot.initrd.availableKernelModules = [ "ata_piix" "vmw_pvscsi" "sd_mod" "sr_mod" ];

  # Adjust to disk size increases
  boot.growPartition = true;

  virtualisation.vmware.guest.enable = true;
  virtualisation.vmware.guest.headless = true;

  nix.settings.trusted-users = [ "tazjin" ];

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/4c51357a-1e34-4b59-b169-63af1fcdce71";
      fsType = "ext4";
    };

  networking = {
    hostName = "polyanka";
    domain = "tazj.in";
    useDHCP = false;

    # Required for VPN usage
    networkmanager.enable = true;

    interfaces.ens192 = {
      ipv4.addresses = lib.singleton {
        address = "159.253.30.129";
        prefixLength = 24;
      };

      ipv6.addresses = lib.singleton {
        address = "2a02:750:7:3305::308";
        prefixLength = 64;
      };
    };

    defaultGateway = "159.253.30.1";
    defaultGateway6.address = "2a02:750:7:3305::1";

    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22 80 443 ];

    nameservers = [
      "79.99.4.100"
      "79.99.4.101"
      "2a02:751:aaaa::1"
      "2a02:751:aaaa::2"
    ];
  };

  time.timeZone = "UTC";

  security.acme.acceptTerms = true;
  security.acme.certs."polyanka.tazj.in" = {
    listenHTTP = ":80";
    email = "mail@tazj.in";
    group = "quassel";
  };

  users.users.tazjin = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
  };

  security.sudo.wheelNeedsPassword = false;

  services.depot.quassel = {
    enable = true;
    acmeHost = "polyanka.tazj.in";
    bindAddresses = [
      "0.0.0.0"
    ];
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

  services.zerotierone.enable = true;
  services.zerotierone.joinNetworks = [
    "35c192ce9bd4c8c7"
  ];

  system.stateVersion = "20.09";
}
