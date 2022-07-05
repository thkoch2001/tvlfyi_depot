# I tried looking up the manufacturer, product name, and version, but
# `dmidecode -t system` reported "To be filled by O.E.M." for each of these
# fields.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXROOT";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/NIXBOOT";
    fsType = "vfat";
  };

  boot = {
    initrd.availableKernelModules = [
      "xhci_pci"
      "ehci_pci"
      "ahci"
      "usb_storage"
      "usbhid"
      "sd_mod"
    ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    # Can verify these settings with:
    # $ lsmod
    # ...or:
    # $ cat /etc/modprobe.d/nixos.conf
    blacklistedKernelModules = [
      # Disabling this buggy network driver (and preferring ethernet) to prevent
      # my machine from becoming unresponsive.
      # TODO(wpcarro): Consider replacing this module with this fork (if NixOS
      # isn't already): https://github.com/tomaspinho/rtl8821ce
      "rtw88_8821ce"
    ];
  };

  swapDevices = [ ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}
