{ modulesPath, config, lib, pkgs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../modules/common.nix
    ../modules/reusable/battery.nix
    ../modules/xserver.nix
    ../modules/fonts.nix
    ../modules/sound.nix
  ];

  networking.hostName = "yeren";

  system.stateVersion = "21.03";

  boot = {
    initrd = {
      availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ ];

      luks.devices = {
        "cryptroot".device = "/dev/disk/by-uuid/dcfbc22d-e0d2-411b-8dd3-96704d3aae2e";
        "cryptswap".device = "/dev/disk/by-uuid/48b8a8fd-559c-4759-a617-56f221cfaaec";
      };
    };

    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/53A9-248B";
      fsType = "vfat";
    };
  };

  swapDevices = [{ device = "/dev/mapper/cryptswap"; }];
}
