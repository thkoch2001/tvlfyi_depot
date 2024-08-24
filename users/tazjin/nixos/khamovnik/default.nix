# Yandex work laptop
#
# Some of the configuration for this machine is not public.
{ depot, lib, pkgs, ... }:

config:
let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
  usermod = name: depot.path.origSrc + ("/users/tazjin/nixos/modules/" + name);
  private = /arc/junk/tazjin;

  zdevice = device: {
    inherit device;
    fsType = "zfs";
  };
in
{
  imports = [
    (usermod "chromium.nix")
    (usermod "desktop.nix")
    (usermod "fonts.nix")
    (usermod "home-config.nix")
    (usermod "laptop.nix")
    (usermod "physical.nix")
    (pkgs.home-manager.src + "/nixos")
  ] ++ (if (builtins.pathExists private) then [
    (private + "/nixos/yandex.nix")
    (private + "/emacs/module.nix")
  ] else [ ]);

  # from hardware-configuration.nix
  boot = {
    initrd.luks.devices."luks-9c3cd590-a648-450d-ae42-ed3859d4c717".device =
      "/dev/disk/by-uuid/9c3cd590-a648-450d-ae42-ed3859d4c717";

    initrd.availableKernelModules = [
      "xhci_pci"
      "thunderbolt"
      "ahci"
      "nvme"
      "usb_storage"
      "sd_mod"
      "rtsx_pci_sdmmc"
    ];
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/1f783029-c4f9-4192-b893-84f4f0c2a493";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/DD01-2B3E";
      fsType = "vfat";
    };
  };

  swapDevices = [{
    device = "/dev/disk/by-uuid/9b9049c5-5975-441d-9ac6-2f9150775fd6";
  }];

  tvl.cache.enable = true;

  networking.hostName = "khamovnik";

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  hardware.graphics.extraPackages = with pkgs; [
    intel-compute-runtime
    intel-media-driver
    intel-vaapi-driver
  ];

  # from generated configuration.nix
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-e9a4b4dc-ade2-45bf-8ed0-0ed5c4c392c9".device = "/dev/disk/by-uuid/e9a4b4dc-ade2-45bf-8ed0-0ed5c4c392c9";
  boot.initrd.luks.devices."luks-e9a4b4dc-ade2-45bf-8ed0-0ed5c4c392c9".keyFile = "/crypto_keyfile.bin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "ru_RU.UTF-8";
    LC_IDENTIFICATION = "ru_RU.UTF-8";
    LC_MEASUREMENT = "ru_RU.UTF-8";
    LC_MONETARY = "ru_RU.UTF-8";
    LC_NAME = "ru_RU.UTF-8";
    LC_NUMERIC = "ru_RU.UTF-8";
    LC_PAPER = "ru_RU.UTF-8";
    LC_TELEPHONE = "ru_RU.UTF-8";
    LC_TIME = "ru_RU.UTF-8";
  };

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Try to work around Intel CPU throttling bugs
  services.throttled.enable = true;

  virtualisation.docker.enable = true;

  hardware.bluetooth.enable = true;
  users.users.tazjin.extraGroups = [ "tss" ];

  environment.systemPackages = with pkgs; [
    tdesktop
    linuxPackages.perf
    hotspot
    protobuf
  ];

  programs.adb.enable = true;

  system.stateVersion = "23.05"; # Did you read the comment?
}
