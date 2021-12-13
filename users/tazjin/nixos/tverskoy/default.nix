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
    fwupd.enable = true;
    printing.enable = true;

    # expose i2c device as /dev/i2c-amdgpu-dm and make it user-accessible
    # this is required for sending control commands to the Dasung screen.
    udev.extraRules = ''
      SUBSYSTEM=="i2c-dev", ACTION=="add", DEVPATH=="/devices/pci0000:00/0000:00:08.1/0000:06:00.0/i2c-5/i2c-dev/i2c-5", SYMLINK+="i2c-amdgpu-dm", TAG+="uaccess"
    '';

    # Configure TLP to keep battery charge between 40-70% while
    # plugged in to the wall (thanks etu for the recommendation).
    tlp = {
      enable = true;
      settings.START_CHARGE_THRESH_BAT0 = 40;
      settings.STOP_CHARGE_THRESH_BAT0 = 70;
    };

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
      third_party.agenix.cli
      third_party.dfmt
    ]) ++

    # programs from nixpkgs
    (with pkgs; [
      amber
      bat
      chromium
      curl
      ddcutil
      direnv
      dmd
      dnsutils
      emacs27-nox # emacsclient
      exa
      fd
      file
      firefox
      gdb
      gh
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
      nmap
      notmuch
      openssh
      openssl
      paperlike-go
      pass
      pavucontrol
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
      tig
      tokei
      tree
      unzip
      vlc
      whois
      xsecurelock
      zoxide
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
          ".elfeed"
          ".gnupg"
          ".local/share/Steam"
          ".local/share/direnv"
          ".local/share/fish"
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
          ".config/mimeapps.list"
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
        lockCmd = "${screenLock}/bin/tazjin-screen-lock";
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

    system.stateVersion = "20.09";
})
