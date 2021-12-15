{ depot, pkgs, lib, ... }:
{ config, ... }:

{
  imports = [
    ./hardware.nix
  ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here.  Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    hostName = "marcus";
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  time.timeZone = "America/New_York";

  services.xserver = {
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
      start = "${depot.users.wpcarro.emacs.nixos}/bin/wpcarros-emacs";
    };
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.mutableUsers = true;
  users.users.wpcarro = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
  };

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

  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    # TODO(wpcarro): Drop dependency on the briefcase concept.
    (depot.users.wpcarro.emacs.nixos { briefcasePath = "$HOME/depot/users/wpcarro"; })
    direnv
    fd
    firefox
    fzf
    git
    ripgrep
    vim
  ];

  system.stateVersion = "21.11";
}
