{ config, lib, pkgs, depot, ... }:

{
  config = {
    boot = {
      loader.grub = {
        enable = true;
        version = 2;
        # TODO(sterni): use /dev/disk/by-id ?
        devices = [
          "/dev/sda"
          "/dev/sdb"
        ];
      };

      kernelModules = [
        "kvm-intel"
      ];

      initrd.availableKernelModules = [
        "ahci"
        "sd_mod"
        "btrfs"
        "realtek"
        "r8169"
      ];
    };

    boot.initrd.luks.devices = {
      "crypt1".device = "/dev/disk/by-uuid/02ac34ee-be10-401b-90c2-1c6aa54c4d5f";
      "crypt2".device = "/dev/disk/by-uuid/7ce07191-e704-4aed-a60f-dfa3ce386b26";
      "crypt-swap1".device = "/dev/disk/by-uuid/fec7155c-6a65-4f25-b271-43763e4c31eb";
      "crypt-swap2".device = "/dev/disk/by-uuid/7b0a03fc-51de-4578-9811-94b00df09d88";
    };

    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/root";
        fsType = "btrfs";
      };

      "/boot" = {
        device = "/dev/disk/by-label/boot";
        fsType = "btrfs";
      };
    };

    swapDevices = [
      { device = "/dev/disk/by-label/swap1"; }
      { device = "/dev/disk/by-label/swap2"; }
    ];

    powerManagement.cpuFreqGovernor = "performance";
    hardware = {
      enableRedistributableFirmware = true;
      cpu.intel.updateMicrocode = true;
    };

    nix.settings = {
      max-jobs = 2;
      cores = 4;
    };
  };
}
