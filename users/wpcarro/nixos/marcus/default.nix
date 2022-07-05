{ depot, pkgs, lib, ... }:
{ ... }:

let
  inherit (depot.users) wpcarro;
  inherit (depot.users.wpcarro.lib) usermod;

  wpcarrosEmacs = wpcarro.emacs.nixos {
    load = [ ./marcus.el ];
  };

  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };
in
{
  imports = [
    (depot.path.origSrc + "/users/wpcarro/nixos/marcus/hardware.nix")
    (pkgs.home-manager.src + "/nixos")
    (usermod "laptop.nix")
  ];

  # Use the TVL binary cache
  tvl.cache.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here.  Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    hostName = "marcus";
    networkmanager.enable = true;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # Schedule daily reboots.
  systemd.timers.auto-reboot = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:00:00";
      Unit = "reboot.target";
    };
  };

  services = wpcarro.common.services // {
    tzupdate.enable = true;

    depot.auto-deploy = {
      enable = true;
      interval = "1d";
    };

    xserver = {
      enable = true;
      libinput = {
        enable = true;
        touchpad.naturalScrolling = false;
        touchpad.tapping = false;
      };
      layout = "us";
      xkbOptions = "caps:escape";
      displayManager = {
        # Give EXWM permission to control the session (from tazjin's setup).
        sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localhost:$USER";
        lightdm.enable = true;
      };
      extraConfig = ''
        Section "InputClass"
            Identifier "Touchscreen catchall"
            MatchIsTouchscreen "on"
            Option "Ignore" "on"
        EndSection
      '';
      windowManager.session = lib.singleton {
        name = "exwm";
        start = "${wpcarrosEmacs}/bin/wpcarros-emacs";
      };
    };
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.mutableUsers = true;
  users.users.wpcarro = {
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
      "video" # needed to control the screen brightness
    ];
    shell = pkgs.fish;
  };

  security.sudo.wheelNeedsPassword = false;

  fonts = {
    fonts = with pkgs; [
      jetbrains-mono
    ];

    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };

  programs = wpcarro.common.programs;

  environment.variables = {
    EDITOR = "emacsclient";
    ALTERNATE_EDITOR = "emacs -q -nw";
    VISUAL = "emacsclient";
  };

  home-manager.useGlobalPkgs = true;
  home-manager.users.wpcarro = { config, lib, ... }: {
    programs.git = {
      enable = true;
      userName = "William Carroll";
      userEmail = "wpcarro@gmail.com";
      extraConfig = {
        pull.rebase = true;
      };
    };

    services.picom = {
      enable = true;
      vSync = true;
      backend = "glx";
    };

    services.redshift = {
      enable = true;
      latitude = 37.4223931;
      longitude = -122.0864016;
    };

    services.dunst.enable = true;
    xdg.configFile."dunst/dunstrc" = {
      source = wpcarro.dotfiles.dunstrc;
      onChange = ''
        ${pkgs.procps}/bin/pkill -u "$USER" ''${VERBOSE+-e} dunst || true
      '';
    };

    systemd.user.startServices = true;
  };

  environment.systemPackages =
    wpcarro.common.shell-utils ++
    (with pkgs; [
      alacritty
      firefox
      pavucontrol
      quasselClient
      tdesktop
      weechat
      wpcarrosEmacs
      xsecurelock
    ]);

  system.stateVersion = "21.11";
}
