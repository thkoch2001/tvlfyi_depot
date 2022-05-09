{ depot, pkgs, lib, ... }:
{ ... }:

let
  inherit (depot.users) wpcarro;

  wpcarrosEmacs = wpcarro.emacs.nixos {
    load = [ ./ava.el ];
  };

  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };
in
{
  imports = [ ./hardware.nix ];

  # Use the TVL binary cache
  tvl.cache.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Support IP forwarding to use this device as a Tailscale exit node.
  boot.kernel.sysctl."net.ipv4.ip_forward" = true;
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;

  time.timeZone = "America/Los_Angeles";

  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here.  Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    hostName = "ava";
    networkmanager.enable = true;
    interfaces.enp1s0.useDHCP = true;
    interfaces.enp3s0.useDHCP = true;
    interfaces.wlp2s0.useDHCP = true;
  };

  services = wpcarro.common.services // {
    openssh.enable = true;

    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "caps:escape";
      displayManager = {
        # Give EXWM permission to control the session (from tazjin's setup).
        sessionCommands = "${pkgs.xorg.xhost}/bin/xhost +SI:localhost:$USER";
        lightdm.enable = true;
      };
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
  users.users.root.openssh.authorizedKeys.keys = [
    wpcarro.keys.nathan
  ];
  users.users.wpcarro = {
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
      "docker"
    ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      wpcarro.keys.nathan
    ];
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

  virtualisation.docker.enable = true;

  environment.variables = {
    EDITOR = "emacsclient";
    ALTERNATE_EDITOR = "emacs -q -nw";
    VISUAL = "emacsclient";
  };

  environment.systemPackages =
    wpcarro.common.shell-utils ++
    (with pkgs; [
      alacritty
      firefox
      google-chrome
      pavucontrol
      quasselClient
      tdesktop
      wpcarrosEmacs
      xsecurelock
    ]);

  system.stateVersion = "21.11";
}
