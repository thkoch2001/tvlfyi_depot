# Home manage configuration for tverskoy.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [ "${depot.third_party.impermanence}/home-manager.nix" ];

  home.persistence."/persist/tazjin/home" = {
    allowOther = true;

    directories = [
      ".cargo"
      ".config/audacity"
      ".config/google-chrome"
      ".config/quassel-irc.org"
      ".config/spotify"
      ".config/syncthing"
      ".config/unity3d"
      ".electrum"
      ".elfeed"
      ".gnupg"
      ".local/share/Steam"
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
