# sanduny.tvl.su
#
# This is a VPS hosted with Bitfolk, intended to additionally serve
# some of our public services like cgit, josh and the websites.
#
# In case of whitby going down, sanduny will keep depot available.

_: # ignore readTree options

{ config, depot, lib, pkgs, ... }:

let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
in
{
  imports = [
    (mod "depot-replica.nix")
    (mod "journaldriver.nix")
    (mod "known-hosts.nix")
    (mod "tvl-cache.nix")
    (mod "tvl-users.nix")
    (mod "www/self-redirect.nix")
  ];

  networking = {
    hostName = "sanduny";
    domain = "tvl.su";
    useDHCP = false;

    interfaces.eth0 = {
      ipv4.addresses = lib.singleton {
        address = "85.119.82.231";
        prefixLength = 21;
      };

      ipv6.addresses = lib.singleton {
        address = "2001:ba8:1f1:f109::feed:edef:beef";
        prefixLength = 64;
      };
    };

    defaultGateway = "85.119.80.1";
    defaultGateway6.address = "2001:ba8:1f1:f109::1";

    firewall.allowedTCPPorts = [ 22 80 443 ];

    # https://bitfolk.com/customer_information.html#toc_2_DNS
    nameservers = [
      "85.119.80.232"
      "85.119.80.233"
      "2001:ba8:1f1:f205::53"
      "2001:ba8:1f1:f206::53"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    emacs-nox
    vim
    curl
    unzip
    htop
  ];

  programs.mtr.enable = true;

  services.openssh.enable = true;
  services.fail2ban.enable = true;

  # Automatically collect garbage from the Nix store.
  services.depot.automatic-gc = {
    enable = true;
    interval = "1 hour";
    diskThreshold = 2; # GiB
    maxFreed = 5; # GiB
    preserveGenerations = "90d";
  };

  # Allow Gerrit to replicate depot to /var/lib/depot
  services.depot.replica.enable = true;

  time.timeZone = "UTC";

  # GRUB does not actually need to be installed on disk; Bitfolk have
  # their own way of booting systems as long as config is in place.
  boot.loader.grub.device = "nodev";
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.initrd.availableKernelModules = [ "xen_blkfront" ];

  hardware.cpu.intel.updateMicrocode = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/aabc3638-43ca-45f3-af89-c451e8448e92";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/75aa99d5-fed7-4c5c-8570-7745f6cff9f5";
      fsType = "ext3";
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/d1721678-c294-482b-b72e-3b15f2c56c63";
      fsType = "ext4";
    };
  };

  tvl.cache.enable = true;

  swapDevices = lib.singleton {
    device = "/dev/disk/by-uuid/df4ad9da-0a06-4c27-93e5-5d44e4750e55";
  };

  system.stateVersion = "22.05"; # Did you read the comment?
}
