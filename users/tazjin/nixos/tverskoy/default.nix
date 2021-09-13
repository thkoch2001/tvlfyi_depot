{ depot, lib, pkgs, ... }:

config: let
  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };

  # Use a screen lock command that resets the keyboard layout
  # before locking, to avoid locking me out when the layout is
  # in Russian.
  screenLock = pkgs.writeShellScriptBin "tazjin-screen-lock" ''
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap us
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:super
    exec ${pkgs.xsecurelock}/bin/xsecurelock
  '';
in lib.fix(self: {
  imports = [
    "${depot.third_party.impermanence}/nixos.nix"
    "${depot.path + "/ops/modules/automatic-gc.nix"}"
    "${pkgs.home-manager.src}/nixos"
  ];

  nix = {
    binaryCachePublicKeys = [
      "cache.tvl.su:kjc6KOMupXc1vHVufJUoDUYeLzbwSr9abcAKdn/U1Jk="
    ];

    binaryCaches = [
      "https://cache.tvl.su"
    ];
  };

  boot = {
    initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
    initrd.kernelModules = [ ];

    # Restore /home to the blank snapshot, erasing all ephemeral data.
    initrd.postDeviceCommands = lib.mkAfter ''
      zfs rollback -r zpool/ephemeral/home@tazjin-clean
    '';

    kernelModules = [ "kvm-amd" "i2c_dev" ];
    extraModulePackages = [ ];
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    zfs.enableUnstable = true;
  };

  fileSystems = {
    "/" =  {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "defaults" "size=4G" "mode=755" ];
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

  fonts = {
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      jetbrains-mono
      noto-fonts-cjk
      noto-fonts-emoji
    ];

    fontconfig = {
      hinting.enable = true;
      subpixel.lcdfilter = "light";

      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };

  environment.persistence."/persist" = {
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
      "/var/cache/mullvad-vpn"
      "/var/lib/bluetooth"
      "/var/lib/systemd/coredump"
      "/var/log"
    ];
    files = [
      "/etc/machine-id"
    ];
  };

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
    tlp.enable = true;
    fwupd.enable = true;

    # expose i2c device as /dev/i2c-amdgpu-dm and make it user-accessible
    udev.extraRules = ''
      SUBSYSTEM=="i2c-dev", ACTION=="add", DEVPATH=="/devices/pci0000:00/0000:00:08.1/0000:06:00.0/i2c-5/i2c-dev/i2c-5", SYMLINK+="i2c-amdgpu-dm", TAG+="uaccess"
    '';

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
      diskThreshold = 42; # GiB
      maxFreed = 100; # GiB
      preserveGenerations = "14d";
    };
  };

  # Automatically detect location to use for redshift
  location.provider = "geoclue2";

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  time.timeZone = "Europe/Moscow";

  users.users.tazjin = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" "video" "adbusers" ];
    uid = 1000;
    shell = pkgs.fish;
    initialHashedPassword = "$6$d3FywUNCuZnJ4l.$ZW2ul59MLYon1v1xhC3lTJZfZ91lWW6Tpi13MpME0cJcYZNrsx7ABdgQRn.K05awruG2Y9ARAzURnmiJ31WTS1";
  };

  programs = {
    fish.enable = true;
    light.enable = true;
    ssh.startAgent = true;
    mosh.enable = true;
    steam.enable = true;
    adb.enable = true;

    # Required by impermanence
    fuse.userAllowOther = true;
  };

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      screenLock
      tools.nsfv-setup
      users.tazjin.emacs
    ]) ++

    # programs from nixpkgs
    (with pkgs; [
      bat
      chromium
      curl
      direnv
      ddcutil
      dnsutils
      emacs27-nox # emacsclient
      exa
      fd
      file
      firefox
      gdb
      git
      gnupg
      google-chrome
      gtk3 # for gtk-launch
      htop
      hyperfine
      iftop
      imagemagick
      jq
      lieer
      manpages
      mosh
      msmtp
      mullvad-vpn
      networkmanagerapplet
      nix-prefetch-github
      notmuch
      openssh
      openssl
      paperlike-go
      pass
      pavucontrol
      picom
      pinentry
      pinentry-emacs
      pulseaudioLight # for pactl
      pwgen
      quasselClient
      rink
      ripgrep
      rustup
      screen
      scrot
      spotify
      syncthing
      tokei
      tree
      vlc
      xsecurelock
    ]);

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
          ".config/google-chrome"
          ".config/quassel-irc.org"
          ".config/spotify"
          ".config/syncthing"
          ".gnupg"
          ".local/share/Steam"
          ".local/share/direnv"
          ".local/share/fish"
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
          ".config/mimeapps.list"
          ".notmuch-config"
        ];
      };

      programs.git = {
        enable = true;
        userName = "Vincent Ambo";
        userEmail = "mail@tazj.in";
        extraConfig = {
          pull.rebase = true;
          init.defaultBranch = "canon";
        };
      };

      services.screen-locker = {
        enable = true;
        enableDetectSleep = true;
        inactiveInterval = 10; # minutes
        lockCmd = "${screenLock}/bin/tazjin-screen-lock";
      };

      systemd.user.startServices = true;
    };

    system.stateVersion = "20.09";
})
