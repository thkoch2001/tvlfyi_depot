# Shared home configuration for all machines.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
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

  # Previous default version, see https://github.com/nix-community/home-manager/blob/master/docs/release-notes/rl-2211.adoc
  home.stateVersion = "18.09";
}
