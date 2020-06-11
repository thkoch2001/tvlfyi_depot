{ depot, lib, ... }:

config: let
  nixpkgs = import depot.third_party.stableNixpkgsSrc {
    config.allowUnfree = true;
  };

  unstable = import depot.third_party.nixpkgsSrc {};
  lieer = (depot.third_party.lieer {});

  # add google-c-style here because other machines get it from, eh,
  # elsewhere.
  frogEmacs = (depot.tools.emacs.overrideEmacs(epkgs: epkgs ++ [
    depot.third_party.emacsPackages.google-c-style
  ]));
in depot.lib.fix(self: {
  # TODO(tazjin): v4l2loopback

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
      "kernel.perf_event_paranoid" = 1;
    };
  };

  hardware = {
    cpu.amd.updateMicrocode = true;
    enableRedistributableFirmware = true;
    pulseaudio.enable = true;
    u2f.enable = true;
    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  nix = {
    maxJobs = 48;
    nixPath = [
      "depot=/depot"
      "nixpkgs=${depot.third_party.nixpkgsSrc}"
    ];
  };

  nixpkgs.pkgs = nixpkgs;

  networking = {
    hostName = "frog";
    useDHCP = false;
    interfaces.enp67s0.useDHCP = true;

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
    extraGroups = [ "wheel" "audio" ];
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

  # Required for Yubikey usage as smartcard
  services.pcscd.enable = true;
  services.udev.packages = [
    nixpkgs.yubikey-personalization
  ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "caps:super";
    exportConfiguration = true;
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

  environment.systemPackages =
    # programs from the depot
    (with depot; [
      fun.idual.script
      lieer
      frogEmacs
      ops.kontemplate
      third_party.ffmpeg
      third_party.git
    ]) ++

    # programs from nixpkgs
    (with nixpkgs; [
      age
      bat
      chromium
      clang-manpages
      clang-tools
      clang_10
      curl
      direnv
      dnsutils
      emacs26 # mostly for emacsclient
      exa
      fd
      gnupg
      go
      google-chrome
      google-cloud-sdk
      htop
      hyperfine
      i3lock
      imagemagick
      jq
      kubectl
      linuxPackages.perf
      miller
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
      rr
      rustup
      scrot
      spotify
      steam
      tokei
      tree
      unzip
      vlc
      xclip
      yubico-piv-tool
      yubikey-personalization
    ]) ++

    # programs from unstable nixpkgs
    (with unstable; [
      zoxide
    ]);

  # ... and other nonsense.
  system.stateVersion = "20.03";
})
