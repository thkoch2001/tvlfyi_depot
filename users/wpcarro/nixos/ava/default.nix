{ depot, pkgs, lib, ... }:
{ ... }:

let
  inherit (depot.users) wpcarro;
  inherit (depot.users.wpcarro.lib) usermod;

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
  imports = [
    (usermod "hardware/nopn.nix")
  ];

  # Use the TVL binary cache
  tvl.cache.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Support IP forwarding to use this device as a Tailscale exit node.
  boot.kernel.sysctl."net.ipv4.ip_forward" = true;
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = true;
  # Additionall exit node settings that Tailscale recommends.
  networking.firewall.checkReversePath = "loose";

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
    # Check the amount of available memory and free swap a few times per second
    # and kill the largest process if both are below 10%.
    earlyoom.enable = true;

    tailscale.enable = true;

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
    wpcarro.keys.tarasco
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
      wpcarro.keys.tarasco
    ];
  };
  users.extraGroups.vboxusers.members = [ "wpcarro" ];

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

  programs = wpcarro.common.programs // {
    mosh.enable = true;
  };

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  environment.variables = {
    EDITOR = "emacsclient";
    ALTERNATE_EDITOR = "emacs -q -nw";
    VISUAL = "emacsclient";
  };

  environment.systemPackages =
    wpcarro.common.shell-utils ++
    (with pkgs; [
      alacritty
      ec2-api-tools
      firefox
      google-chrome
      httpie
      pavucontrol
      quasselClient
      remmina
      tdesktop
      wpcarrosEmacs
      xsecurelock
    ]);

  system.stateVersion = "21.11";
}
