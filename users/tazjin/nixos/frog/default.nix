{ depot, lib, ... }:

config: let
  nixpkgs = import depot.third_party.nixpkgsSrc {
    config.allowUnfree = true;
  };

  lieer = depot.third_party.lieer {};

  # add google-c-style here because other machines get it from, eh,
  # elsewhere.
  frogEmacs = (depot.users.tazjin.emacs.overrideEmacs(epkgs: epkgs ++ [
    depot.third_party.emacsPackages.google-c-style
  ]));

  quasselClient = depot.third_party.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };
in depot.lib.fix(self: {
  imports = [
    "${depot.depotPath}/ops/nixos/v4l2loopback.nix"
  ];

  boot = {
    tmpOnTmpfs = true;
    kernelModules = [ "kvm-amd" ];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      luks.devices.frog-crypt.device = "/dev/disk/by-label/frog-crypt";
      availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [ "dm-snapshot" ];
    };

    kernelPackages = nixpkgs.linuxPackages_latest;
    kernel.sysctl = {
      "kernel.perf_event_paranoid" = -1;
    };

    kernelPatches = [
      depot.third_party.kernelPatches.trx40_usb_audio
    ];
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = true;
      package = nixpkgs.pulseaudioFull;
    };

    bluetooth = {
      enable = true;
    };
  };

  nix = {
    maxJobs = 48;
    nixPath = [
      "depot=/depot"
      "nixpkgs=${depot.third_party.nixpkgsSrc}"
    ];

    binaryCaches = ["ssh://nix-ssh@whitby.tvl.fyi"];
    binaryCachePublicKeys = ["cache.tvl.fyi:fd+9d1ceCPvDX/xVhcfv8nAa6njEhAGAEe+oGJDEeoc="];
  };

  nixpkgs.pkgs = nixpkgs;

  networking = {
    hostName = "frog";
    useDHCP = true;

    # Don't use ISP's DNS servers:
    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

    firewall.enable = false;
  };

  # Generate an immutable /etc/resolv.conf from the nameserver settings
  # above (otherwise DHCP overwrites it):
  environment.etc."resolv.conf" = with lib; {
    source = depot.third_party.writeText "resolv.conf" ''
      ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") self.networking.nameservers)}
      options edns0
    '';
  };

  time.timeZone = "Europe/London";

  fileSystems = {
    "/".device = "/dev/disk/by-label/frog-root";
    "/boot".device = "/dev/disk/by-label/BOOT";
    "/home".device = "/dev/disk/by-label/frog-home";
  };

  # Configure user account
  users.extraUsers.tazjin = {
    extraGroups = [ "wheel" "audio" "docker" ];
    isNormalUser = true;
    uid = 1000;
    shell = nixpkgs.fish;
  };

  security.sudo = {
    enable = true;
    extraConfig = "wheel ALL=(ALL:ALL) SETENV: ALL";
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

  # Configure location (Vauxhall, London) for services that need it.
  location = {
    latitude = 51.4819109;
    longitude = -0.1252998;
  };

  programs.fish.enable = true;
  programs.ssh.startAgent = true;

  services.redshift.enable = true;
  services.openssh.enable = true;
  services.fstrim.enable = true;
  services.blueman.enable = true;

  # Required for Yubikey usage as smartcard
  services.pcscd.enable = true;
  services.udev.packages = [
    nixpkgs.yubikey-personalization
  ];

  # Enable Docker for Nixery testing
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "caps:super";
    exportConfiguration = true;
    videoDrivers = [ "amdgpu" ];
    displayManager = {
      # Give EXWM permission to control the session.
      sessionCommands = "${nixpkgs.xorg.xhost}/bin/xhost +SI:localuser:$USER";

      lightdm.enable = true;
      lightdm.greeters.gtk.clock-format = "%H·%M"; # TODO(tazjin): TZ?
    };

    windowManager.session = lib.singleton {
      name = "exwm";
      start = "${frogEmacs}/bin/tazjins-emacs";
    };
  };

  # Do not restart the display manager automatically
  systemd.services.display-manager.restartIfChanged = lib.mkForce false;

  # clangd needs more than ~2GB in the runtime directory to start up
  services.logind.extraConfig = ''
    RuntimeDirectorySize=16G
  '';

  # Configure email setup
  systemd.user.services.lieer-tazjin = {
    description = "Synchronise mail@tazj.in via lieer";
    script = "${lieer}/bin/gmi sync";

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

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      frogEmacs
      fun.idual.script
      fun.uggc
      lieer
      ops.kontemplate
      quasselClient
      third_party.ffmpeg
      third_party.git
      third_party.lutris
      third_party.rr
      tools.nsfv-setup
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      age
      bat
      chromium
      clang-manpages
      clang-tools
      clang_11
      curl
      direnv
      dnsutils
      emacs26 # mostly for emacsclient
      exa
      fd
      file
      gdb
      gnupg
      go
      google-chrome
      google-cloud-sdk
      htop
      hyperfine
      i3lock
      iftop
      imagemagick
      jq
      kubectl
      linuxPackages.perf
      manpages
      miller
      msmtp
      nix-prefetch-github
      notmuch
      obs-studio
      obs-v4l2sink
      openssh
      openssl
      pass
      pavucontrol
      pciutils
      pinentry
      pinentry-emacs
      pmutils
      pwgen
      ripgrep
      rustup
      screen
      scrot
      spotify
      steam
      tokei
      transmission
      tree
      unzip
      usbutils
      v4l-utils
      vlc
      xclip
      xsecurelock
      yubico-piv-tool
      yubikey-personalization
      zoxide
    ]);

  # ... and other nonsense.
  system.stateVersion = "20.03";
})
