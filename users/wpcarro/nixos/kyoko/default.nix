{ depot, pkgs, lib, ... }:
_:

let
  inherit (depot.users) wpcarro;
  inherit (depot.users.wpcarro.lib) usermod;

  wpcarrosEmacs = wpcarro.emacs.nixos {
    load = [ ./kyoko.el ];
  };

  quasselClient = pkgs.quassel.override {
    client = true;
    enableDaemon = false;
    monolithic = false;
  };
in
{
  imports = [
    (usermod "hardware/dell-emc-egw-5200.nix")
    (usermod "hadrian-cache.nix")
  ];

  # TVL's Nix binary cache
  tvl.cache.enable = true;

  # Hadrian's Nix binary cache.
  hadrian.cache.enable = true;

  nix.settings.trusted-users = [ "@wheel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Additionall exit node settings that Tailscale recommends.
  networking.firewall.checkReversePath = "loose";

  time.timeZone = "America/Los_Angeles";

  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here.  Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    hostName = "kyoko";
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

    printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint ];
    };

    xserver = {
      enable = true;
      xkb.layout = "us";
      xkb.options = "caps:escape";
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
  hardware.pulseaudio.enable = true;

  users.mutableUsers = true;
  users.users.root.openssh.authorizedKeys.keys = with wpcarro.keys; [
    iphone
    nathan
    tarasco
  ];
  users.users.wpcarro = {
    initialPassword = "password";
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
      "docker"
    ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = with wpcarro.keys; [
      iphone
      nathan
      tarasco
    ];
  };
  users.extraGroups.vboxusers.members = [ "wpcarro" ];

  security.sudo.wheelNeedsPassword = false;

  fonts = {
    packages = with pkgs; [
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
