{ config, lib, pkgs, ... }:
{
  imports = [
    ../modules/reusable/battery.nix
    <nixos-hardware/common/cpu/intel>
    <nixos-hardware/common/pc/laptop>
  ];

  networking.hostName = "chupacabra";

  powerManagement = {
    enable = true;
    powertop.enable = true;
    cpuFreqGovernor = "performance";
  };

  laptop.onLowBattery = {
    enable = true;
    action = "hibernate";
    thresholdPercentage = 5;
  };

  boot.initrd.luks.devices."cryptswap".device = "/dev/disk/by-uuid/3b6e2fd4-bfe9-4392-a6e0-4f3b3b76e019";

  boot.kernelParams = [ "acpi_rev_override" ];
  services.thermald.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  # Intel-only graphics
  hardware.nvidiaOptimus.disable = true;
  boot.blacklistedKernelModules = [ "nouveau" "intel" ];
  services.xserver.videoDrivers = [ "intel" ];

  # Nvidia Optimus (hybrid) - currently not working
  # services.xserver.videoDrivers = [ "intel" "nvidia" ];
  # boot.blacklistedKernelModules = [ "nouveau" "bbswitch" ];
  # boot.extraModulePackages = [ pkgs.linuxPackages.nvidia_x11 ];
  # hardware.bumblebee.enable = true;
  # hardware.bumblebee.pmMethod = "none";

  systemd.services.disable-usb-autosuspend = {
    description = "Disable USB autosuspend";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = { Type = "oneshot"; };
    unitConfig.RequiresMountsFor = "/sys";
    script = ''
      echo -1 > /sys/module/usbcore/parameters/autosuspend
    '';
  };
}
