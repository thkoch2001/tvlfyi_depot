{ depot, lib, pkgs, ... }:

config: let
  nixpkgs = import pkgs.path {
    config.allowUnfree = true;
  };

  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };

  # Use a screen lock command that resets the keyboard layout
  # before locking, to avoid locking me out when the layout is
  # in Russian.
  screenLock = nixpkgs.writeShellScriptBin "tazjin-screen-lock" ''
    ${nixpkgs.xorg.setxkbmap}/bin/setxkbmap us
    ${nixpkgs.xorg.setxkbmap}/bin/setxkbmap -option caps:super
    exec ${nixpkgs.xsecurelock}/bin/xsecurelock
  '';
in lib.fix(self: {
  imports = [
    "${depot.third_party.impermanence}/nixos.nix"
    "${nixpkgs.home-manager.src}/nixos"
  ];

  nix = {
    nixPath = lib.mkForce [
      "nixpkgs=${pkgs.path}"
      "nixos=${pkgs.path}"
      "depot=/depot"
    ];

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

    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    kernelPackages = nixpkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
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
    fonts = with nixpkgs; [
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

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:super";
      videoDrivers = [ "amdgpu" ];

      libinput.enable = true;

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${nixpkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";
        lightdm.enable = true;
        # lightdm.greeters.gtk.clock-format = "%H:%M"; # TODO(tazjin): TZ?
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${depot.users.tazjin.emacs}/bin/tazjins-emacs";
      };
    };
  };

  # Automatically detect location to use for redshift
  location.provider = "geoclue2";

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  time.timeZone = "Africa/Cairo";

  users.users.tazjin = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "networkmanager" "video" ];
    uid = 1000;
    shell = nixpkgs.fish;
    initialHashedPassword = "$6$d3FywUNCuZnJ4l.$ZW2ul59MLYon1v1xhC3lTJZfZ91lWW6Tpi13MpME0cJcYZNrsx7ABdgQRn.K05awruG2Y9ARAzURnmiJ31WTS1";
  };

  programs = {
    fish.enable = true;
    light.enable = true;
    ssh.startAgent = true;
    mosh.enable = true;

    # Required by impermanence
    fuse.userAllowOther = true;
  };

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      screenLock
      third_party.lieer
      tools.nsfv-setup
      users.tazjin.emacs
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      bat
      chromium
      curl
      direnv
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
      htop
      hyperfine
      iftop
      imagemagick
      jq
      manpages
      mosh
      msmtp
      mullvad-vpn
      networkmanagerapplet
      nix-prefetch-github
      notmuch
      openssh
      openssl
      pass
      pavucontrol
      pinentry
      pinentry-emacs
      pulseaudioLight # for pactl
      pwgen
      quasselClient
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
          ".local/share/direnv"
          ".local/share/fish"
          ".mozilla/firefox"
          ".password-store"
          ".rustup"
          ".ssh"
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
