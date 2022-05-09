{ depot, lib, pkgs, ... }:

config:
let
  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };

  mod = name: depot.path + ("/ops/modules/" + name);
  usermod = name: depot.path + ("/users/tazjin/nixos/modules/" + name);
in
lib.fix (self: {
  imports = [
    (mod "open_eid.nix")
    (usermod "physical.nix")
    (usermod "fonts.nix")
    "${depot.third_party.impermanence}/nixos.nix"
    "${pkgs.home-manager.src}/nixos"
  ] ++ lib.optional (builtins.pathExists ./local-config.nix) ./local-config.nix;

  tvl.cache.enable = true;

  boot = rec {
    initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
    initrd.kernelModules = [ ];

    # Restore /home to the blank snapshot, erasing all ephemeral data.
    initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r zpool/ephemeral/home@tazjin-clean
    '';

    # Install thinkpad modules for TLP
    extraModulePackages = [ kernelPackages.acpi_call ];

    kernelModules = [ "kvm-amd" "i2c_dev" ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    zfs.enableUnstable = true;
  };

  fileSystems = {
    "/" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "defaults" "size=8G" "mode=755" ];
    };

    "/home" = {
      device = "zpool/ephemeral/home";
      fsType = "zfs";
    };

    "/nix" = {
      device = "zpool/local/nix";
      fsType = "zfs";
    };

    "/depot" = {
      device = "zpool/safe/depot";
      fsType = "zfs";
    };

    "/persist" = {
      device = "zpool/safe/persist";
      fsType = "zfs";
      neededForBoot = true;
    };

    # SD card
    "/mnt" = {
      device = "/dev/disk/by-uuid/c602d703-f1b9-4a44-9e45-94dfe24bdaa8";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/BF4F-388B";
      fsType = "vfat";
    };
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    bluetooth.enable = true;

    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };

  networking = {
    hostName = "tverskoy";
    hostId = "3c91827f";
    domain = "tvl.su";
    useDHCP = false;
    networkmanager.enable = true;
    firewall.enable = false;

    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];
  };

  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
      "/var/cache/mullvad-vpn"
      "/var/lib/bluetooth"
      "/var/lib/systemd/coredump"
      "/var/lib/tailscale"
      "/var/lib/zerotier-one"
      "/var/log"
    ];

    files = [
      "/etc/machine-id"
    ];
  };

  # from https://github.com/NixOS/nixpkgs/issues/64965
  environment.etc."ipsec.secrets".text = ''
    include ipsec.d/ipsec.nm-l2tp.secrets
  '';

  security.rtkit.enable = true;

  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    redshift.enable = true;
    blueman.enable = true;
    mullvad-vpn.enable = true;
    fwupd.enable = true;
    printing.enable = true;

    # expose i2c device as /dev/i2c-amdgpu-dm and make it user-accessible
    # this is required for sending control commands to the Dasung screen.
    udev.extraRules = ''
      SUBSYSTEM=="i2c-dev", ACTION=="add", DEVPATH=="/devices/pci0000:00/0000:00:08.1/0000:06:00.0/i2c-5/i2c-dev/i2c-5", SYMLINK+="i2c-amdgpu-dm", TAG+="uaccess"
    '';

    # Enable power-saving features.
    tlp.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:super";
      videoDrivers = [ "amdgpu" ];

      libinput.enable = true;

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";
        lightdm.enable = true;
        # lightdm.greeters.gtk.clock-format = "%H:%M"; # TODO(tazjin): TZ?
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${depot.users.tazjin.emacs}/bin/tazjins-emacs";
      };
    };

    # Automatically collect garbage from the Nix store.
    depot.automatic-gc = {
      enable = true;
      interval = "1 hour";
      diskThreshold = 16; # GiB
      maxFreed = 10; # GiB
      preserveGenerations = "14d";
    };
  };

  # Set variables to enable EXWM-XIM
  environment.sessionVariables = {
    XMODIFIERS = "@im=exwm-xim";
    GTK_IM_MODULE = "xim";
    QT_IM_MODULE = "xim";
    CLUTTER_IM_MODULE = "xim";
  };

  # Automatically detect location to use for redshift
  location.provider = "geoclue2";

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # If something needs more than 10s to stop it should probably be
  # killed.
  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  time.timeZone = "Africa/Cairo";

  nix = {
    trustedUsers = [ "tazjin" ];
  };

  users.users.tazjin = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" "video" "adbusers" ];
    uid = 1000;
    shell = pkgs.fish;
    initialHashedPassword = "$6$d3FywUNCuZnJ4l.$ZW2ul59MLYon1v1xhC3lTJZfZ91lWW6Tpi13MpME0cJcYZNrsx7ABdgQRn.K05awruG2Y9ARAzURnmiJ31WTS1";
  };

  programs = {
    adb.enable = true;
    fish.enable = true;
    light.enable = true;
    mosh.enable = true;
    ssh.startAgent = true;

    # Required by impermanence
    fuse.userAllowOther = true;
  };

  systemd.user.services.lieer-tazjin = {
    description = "Synchronise mail@tazj.in via lieer";
    script = "${pkgs.lieer}/bin/gmi sync";

    serviceConfig = {
      WorkingDirectory = "%h/mail/account.tazjin";
      Type = "oneshot";
    };
  };

  systemd.user.timers.lieer-tazjin = {
    wantedBy = [ "timers.target" ];

    timerConfig = {
      OnActiveSec = "1";
      OnUnitActiveSec = "180";
    };
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.tazjin = { config, lib, ... }: {
    imports = [ "${depot.third_party.impermanence}/home-manager.nix" ];

    home.persistence."/persist/tazjin/home" = {
      allowOther = true;

      directories = [
        ".cargo"
        ".config/audacity"
        ".config/google-chrome"
        ".config/quassel-irc.org"
        ".config/spotify"
        ".config/syncthing"
        ".config/unity3d"
        ".electrum"
        ".elfeed"
        ".gnupg"
        ".local/share/Steam"
        ".local/share/audacity"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/keyrings"
        ".local/share/zoxide"
        ".mozilla/firefox"
        ".password-store"
        ".rustup"
        ".ssh"
        ".steam"
        ".telega"
        "go"
        "mail"
      ];

      files = [
        ".notmuch-config"
      ];
    };

    home.activation.screenshots = lib.hm.dag.entryAnywhere ''
      $DRY_RUN_CMD mkdir -p $HOME/screenshots
    '';

    programs.git = {
      enable = true;
      userName = "Vincent Ambo";
      userEmail = "mail@tazj.in";
      extraConfig = {
        pull.rebase = true;
        init.defaultBranch = "canon";
      };
    };

    programs.fish = {
      enable = true;
      interactiveShellInit = ''
        ${pkgs.zoxide}/bin/zoxide init fish | source
      '';
    };

    services.screen-locker = {
      enable = true;
      enableDetectSleep = true;
      inactiveInterval = 10; # minutes
      lockCmd = "${depot.users.tazjin.screenLock}/bin/tazjin-screen-lock";
    };

    services.picom = {
      enable = true;
      vSync = true;
      backend = "glx";
    };

    # Enable the dunst notification daemon, but force the
    # configuration file separately instead of going via the strange
    # Nix->dunstrc encoding route.
    services.dunst.enable = true;
    xdg.configFile."dunst/dunstrc" = {
      source = depot.users.tazjin.dotfiles.dunstrc;
      onChange = ''
        ${pkgs.procps}/bin/pkill -u "$USER" ''${VERBOSE+-e} dunst || true
      '';
    };

    systemd.user.startServices = true;
  };

  services.tailscale.enable = true;

  services.zerotierone.enable = true;
  services.zerotierone.joinNetworks = [
    "35c192ce9bd4c8c7"
  ];

  system.stateVersion = "20.09";
})
