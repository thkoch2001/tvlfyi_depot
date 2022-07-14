# Shared home configuration for all machines.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [ (depot.third_party.sources.impermanence + "/home-manager.nix") ];

  home.persistence."/persist/tazjin/home" = {
    allowOther = true;

    directories = [
      ".cargo"
      ".config/audacity"
      ".config/google-chrome"
      ".config/quassel-irc.org"
      ".config/unity3d"
      ".electrum"
      ".gnupg"
      ".local/share/audacity"
      ".local/share/direnv"
      ".local/share/fish"
      ".local/share/keyrings"
      ".local/share/zoxide"
      ".mozilla/firefox"
      ".password-store"
      ".rustup"
      ".ssh"
      ".steam"
      ".telega"
      ".thunderbird"
      "go"
      "mail"
    ];

    files = [
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
      safe.directory = [ "/depot" ];
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
    lockCmd = "${depot.users.tazjin.screenLock}/bin/tazjin-screen-lock";
  };

  services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";
  };

  services.syncthing.enable = true;

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
}
