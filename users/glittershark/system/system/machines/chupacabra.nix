{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/common.nix
    ../modules/reusable/battery.nix
    ../modules/tvl.nix
    ../modules/fcitx.nix
    ../modules/rtlsdr.nix
    ../../../../../ops/nixos/v4l2loopback.nix
    ../modules/desktop.nix
    ../modules/development.nix
  ];

  hardware.enableRedistributableFirmware = true;

  networking.hostName = "chupacabra";

  powerManagement = {
    enable = true;
    powertop.enable = true;
    cpuFreqGovernor = "powersave";
  };

  laptop.onLowBattery = {
    enable = true;
    action = "hibernate";
    thresholdPercentage = 5;
  };

  boot = {
    blacklistedKernelModules = [ "nouveau" "intel" ];
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
      kernelModules = [ ];

      luks.devices = {
        "cryptroot".device = "/dev/disk/by-uuid/c2fc7ce7-a45e-48a1-8cde-be966ef601db";
        "cryptswap".device = "/dev/disk/by-uuid/3b6e2fd4-bfe9-4392-a6e0-4f3b3b76e019";
      };
    };

    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "acpi_rev_override" ];

    kernel.sysctl = {
      "kernel.perf_event_paranoid" = -1;
      "vm.swappiness" = 1;
    };
  };

  services.thermald.enable = true;

  hardware.cpu.intel.updateMicrocode = true;

  # Intel-only graphics
  hardware.nvidiaOptimus.disable = true;
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

  # From hardware-configuration.nix

  fileSystems."/" =
    { device = "/dev/mapper/cryptroot";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/3492-9E3A";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/caa7e2ff-475b-4024-b29e-4f88f733fc4c"; }
    ];

  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # from nixos-hardware TODO sort this around
  services.tlp.enable = true;
  services.fstrim.enable = lib.mkDefault true;

  # Intel cpu stuff
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  services.udev.extraRules = ''
    # UDEV rules for Teensy USB devices
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
  '';

  # Necessary to get steam working
  hardware.opengl.driSupport32Bit = true;

  nix = {
    maxJobs = lib.mkDefault 12;
    binaryCaches = [ "ssh://grfn@172.16.0.5" ];
    trustedBinaryCaches = [ "ssh://grfn@172.16.0.5" ];
    buildMachines = [
      {
        hostName = "172.16.0.4";
        sshUser = "griffin";
        sshKey = "/home/grfn/.ssh/id_rsa";
        system = "x86_64-darwin";
        maxJobs = 8; # 16 cpus
      }
      {
        hostName = "172.16.0.3";
        sshUser = "griffin";
        sshKey = "/home/grfn/.ssh/id_rsa";
        system = "x86_64-darwin";
        maxJobs = 4;
      }
    ];
  };
}
