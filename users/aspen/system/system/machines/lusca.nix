{ depot, modulesPath, config, lib, pkgs, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../modules/common.nix
    ../modules/laptop.nix
    ../modules/xserver.nix
    ../modules/fonts.nix
    ../modules/sound.nix
    ../modules/tvl.nix
    ../modules/development.nix
  ];

  networking.hostName = "lusca";

  system.stateVersion = "24.05";

  time.timeZone = "America/New_York";

  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  boot = {
    initrd = {
      availableKernelModules =
        [ "nvme" "xhci_pci" "thunderbolt" "usb_storage" "sd_mod" ];
      kernelModules = [ ];

      luks.devices."cryptroot".device =
        "/dev/disk/by-uuid/9e525746-5bca-4451-8710-a6f0e09b751c";
    };

    kernelModules = [ "kvm-amd" ];

    kernelParams = [
      "resume=LABEL=SWAP"
      "resume_offset=795904" # sudo btrfs inspect-internal map-swapfile -r /swap/swapfile
    ];

    resumeDevice = "/dev/disk/by-uuid/4c099cee-8d42-49c1-916c-62a0b5effbd2";

    kernel.sysctl = { "kernel.perf_event_paranoid" = -1; };
  };

  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4c099cee-8d42-49c1-916c-62a0b5effbd2";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };

    "/home" = {
      device = "/dev/disk/by-uuid/4c099cee-8d42-49c1-916c-62a0b5effbd2";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };

    "/nix" = {
      device = "/dev/disk/by-uuid/4c099cee-8d42-49c1-916c-62a0b5effbd2";
      fsType = "btrfs";
      options = [ "subvol=nix" ];
    };

    "/swap" = {
      device = "/dev/disk/by-uuid/4c099cee-8d42-49c1-916c-62a0b5effbd2";
      fsType = "btrfs";
      options = [ "subvol=swap" ];
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/0E7D-3C3F";
      fsType = "vfat";
    };
  };

  swapDevices = [{ device = "/swap/swapfile"; }];

  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
    SuspendState=mem
  '';

  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };

  services.tailscale.enable = true;

  services.fwupd = {
    enable = true;
    extraRemotes = [ "lvfs-testing" ];
  };

  services.tlp.enable = lib.mkForce false;
  services.power-profiles-daemon.enable = true;

  services.thermald.enable = true;

  services.fprintd.enable = true;
  security.pam.services = {
    login.fprintAuth = true;
    sudo.fprintAuth = true;
    i3lock.fprintAuth = true;
    i3lock-color.fprintAuth = true;
    lightdm.fprintAuth = true;
    lightdm-greeter.fprintAuth = true;
  };

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id.indexOf("net.reactivated.fprint.") == 0 || action.id.indexOf("net.reactivated.Fprint.") == 0) {
          polkit.log("action=" + action);
          polkit.log("subject=" + subject);
          return polkit.Result.YES;
      }
    });
  '';

  services.udev.extraRules = ''
    # Ethernet expansion card support
    ACTION=="add", SUBSYSTEM=="usb", ATTR{idVendor}=="0bda", ATTR{idProduct}=="8156", ATTR{power/autosuspend}="20"
  '';

  hardware.sensor.iio.enable = true;

  hardware.graphics.enable32Bit = true;

  # TPM
  security.tpm2 = {
    enable = true;
    pkcs11.enable = true;
    tctiEnvironment.enable = true;
  };
  users.users.aspen.extraGroups = [ "tss" ];
}
