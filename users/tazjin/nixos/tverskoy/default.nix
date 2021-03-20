{ depot, lib, ... }:

config: let
  # add google-c-style here because other machines get it from, eh,
  # elsewhere.
  emacs = (depot.users.tazjin.emacs.overrideEmacs(epkgs: epkgs ++ [
    depot.third_party.emacsPackages.google-c-style
  ]));
  nixpkgs = import depot.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };
in lib.fix(self: {
  boot = {
    initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
    initrd.kernelModules = [ ];
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
    bluetooth.hsphfpd.enable = true;
  };

  networking = {
    hostName = "tverskoy";
    hostId = "3c91827f";
    useDHCP = false;
    networkmanager.enable = true;

    # wireless = {
    #   enable = true;
    #   networks.Rumpetroll = { psk = "fisk1234"; };
    #   networks."How do I computer?" = { psk = "washyourface"; };
    # };

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

  security.rtkit.enable = true;
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
    };

    # redshift.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:super";
      videoDrivers = [ "amdgpu" ];

      displayManager = {
        # Give EXWM permission to control the session.
        sessionCommands = "${nixpkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";
        lightdm.enable = true;
        # lightdm.greeters.gtk.clock-format = "%H:%M"; # TODO(tazjin): TZ?
      };

      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${emacs}/bin/tazjins-emacs";
      };
    };
  };

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  time.timeZone = "Africa/Cairo";

  users.users.tazjin = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
    shell = nixpkgs.fish;
    initialHashedPassword = "$6$d3FywUNCuZnJ4l.$ZW2ul59MLYon1v1xhC3lTJZfZ91lWW6Tpi13MpME0cJcYZNrsx7ABdgQRn.K05awruG2Y9ARAzURnmiJ31WTS1";
  };

  programs = {
    light.enable = true;
  };

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      emacs
      tools.nsfv-setup
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      bat
      chromium
      curl
      direnv
      emacs27-nox # emacsclient
      exa
      fd
      file
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
      msmtp
      nix-prefetch-github
      notmuch
      openssh
      openssl
      pass
      pavucontrol
      pinentry
      pinentry-emacs
      pwgen
      ripgrep
      rustup
      screen
      scrot
      tokei
      tree
      vlc
      xsecurelock
    ]);

  system.stateVersion = "20.09";
})
