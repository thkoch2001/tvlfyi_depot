{ depot, modulesPath, config, lib, pkgs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../modules/common.nix
    ../modules/xserver.nix
    ../modules/fonts.nix
    ../modules/sound.nix
    ../modules/tvl.nix
    ../modules/development.nix
    ../modules/work/kolide.nix
  ];

  networking.hostName = "ogopogo";

  system.stateVersion = "22.11";

  boot = {
    initrd = {
      availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
      kernelModules = [ ];
    };

    kernelPackages = pkgs.linuxPackages_5_15;

    kernelModules = [ "kvm-amd" ];
    blacklistedKernelModules = [ ];
    extraModulePackages = [ ];

    kernel.sysctl = {
      "kernel.perf_event_paranoid" = -1;
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/d67506cf-7039-484d-97c0-00321a7858dc";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/AE73-03A3";
      fsType = "vfat";
    };
  };

  swapDevices = [{
    device = "/dev/disk/by-uuid/8bdae7c8-5160-491f-8cd0-4f0a79acadf9";
  }];

  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };

  nixpkgs.config.allowUnfree = true;
  hardware.enableAllFirmware = true;

  hardware.pulseaudio.extraConfig = ''
    load-module module-remap-source source_name=KompleteAudio6_1 source_properties=device.description=KompleteAudio6Input1 master=alsa_input.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.multichannel-input remix=no channels=1 master_channel_map=front-left channel_map=mono
    load-module module-remap-source source_name=KompleteAudio6_2 source_properties=device.description=KompleteAudio6Input2 master=alsa_input.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.multichannel-input remix=no channels=1 master_channel_map=front-right channel_map=mono
    load-module module-remap-sink sink_name=KompleteAudio6_12 sink_properties=device.description=KompleteAudio6_12 remix=no master=alsa_output.usb-Native_Instruments_Komplete_Audio_6_458E0FFD-00.analog-surround-21 channels=2 master_channel_map=front-left,front-right channel_map=front-left,front-right
  '';

  services.fwupd.enable = true;

  services.tailscale.enable = true;
}
