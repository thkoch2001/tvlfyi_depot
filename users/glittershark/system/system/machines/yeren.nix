{ modulesPath, config, lib, pkgs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../modules/common.nix
    ../modules/reusable/battery.nix
    ../modules/xserver.nix
    ../modules/fonts.nix
    ../modules/sound.nix
    ../modules/tvl.nix
    ../modules/development.nix
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
    kernelParams = [
      "i915.preliminary_hw_support=1"
    ];
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

  services.xserver = {
    exportConfiguration = true;
    extraConfig = ''
      Section "Device"
        Identifier  "Intel Graphics"
        Driver      "intel"
        Option      "TripleBuffer" "true"
        Option      "TearFree"     "true"
        Option      "DRI"          "true"
        Option      "AccelMethod"  "sna"
      EndSection
    '';
  };

  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  services.fprintd = {
    enable = true;
    package = config.depot.users.glittershark.pkgs.fprintd;
  };

  security.pam.services = {
    login.fprintAuth = true;
    sudo.fprintAuth = true;
    i3lock.fprintAuth = false;
    i3lock-color.fprintAuth = false;
    lightdm.fprintAuth = true;
    lightdm-greeter.fprintAuth = true;
  };

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.extraConfig = ''
    load-module module-remap-source source_name=KompleteAudio6_1 source_properties=device.description=KompleteAudio6Input1 master=alsa_input.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.multichannel-input remix=no channels=1 master_channel_map=front-left channel_map=mono
    load-module module-remap-source source_name=KompleteAudio6_2 source_properties=device.description=KompleteAudio6Input2 master=alsa_input.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.multichannel-input remix=no channels=1 master_channel_map=front-right channel_map=mono
    load-module module-remap-sink sink_name=KompleteAudio6_12 sink_properties=device.description=KompleteAudio6_12 remix=no master=alsa_output.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.analog-surround-21 channels=2 master_channel_map=front-left,front-right channel_map=front-left,front-right
  '';
}
