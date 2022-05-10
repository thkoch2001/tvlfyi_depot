# zamalek is my Huawei MateBook X (unknown year)
{ depot, lib, pkgs, ... }:

config:
let
  mod = name: depot.path + ("/ops/modules/" + name);
  usermod = name: depot.path + ("/users/tazjin/nixos/modules/" + name);

  zdevice = device: {
    inherit device;
    fsType = "zfs";
  };
in
{
  imports = [
    (usermod "desktop.nix")
    (usermod "fonts.nix")
    (usermod "home-config.nix")
    (usermod "laptop.nix")
    (usermod "persistence.nix")
    (usermod "physical.nix")
    (usermod "zerotier.nix")

    (depot.third_party.impermanence + "/nixos.nix")
    (pkgs.home-manager.src + "/nixos")
  ];

  tvl.cache.enable = true;

  boot = {
    initrd.availableKernelModules = [ "nvme" "xhci_pci" ];
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    supportedFilesystems = [ "zfs" ];
    zfs.devNodes = "/dev/";

    extraModprobeConfig = ''
      options snd_hda_intel power_save=1
      options iwlwifi power_save=1
      options iwldvm force_cam=0
      options i915 enable_guc=3 enable_fbc=1
    '';
  };

  fileSystems = {
    "/" = zdevice "zpool/ephemeral/root";
    "/home" = zdevice "zpool/ephemeral/home";
    "/persist" = zdevice "zpool/persistent/data" // { neededForBoot = true; };
    "/nix" = zdevice "zpool/persistent/nix";
    "/depot" = zdevice "zpool/persistent/depot";

    "/boot" = {
      device = "/dev/disk/by-uuid/2487-3908";
      fsType = "vfat";
    };
  };

  networking = {
    hostName = "zamalek";
    domain = "tvl.su";
    hostId = "ee399356";
    networkmanager.enable = true;

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    opengl.enable = true;
  };

  system.stateVersion = "21.11";
}
