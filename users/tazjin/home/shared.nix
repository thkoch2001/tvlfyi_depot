# Shared home configuration for all machines.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager


let
  # URL handler to open `tg://` URLs in telega.el
  telega-launcher = pkgs.writeShellScriptBin "telega-launcher" ''
    echo "Opening ''${1} in telega.el ..."
    ${depot.users.tazjin.emacs.emacs}/bin/emacsclient -e "(telega-browse-url \"''${1}\")"
  '';
in
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
      # emacs vterm integration
      source (find '${pkgs.emacsPackages.vterm}' -name 'emacs-vterm.fish')

      # z
      ${pkgs.zoxide}/bin/zoxide init fish | source
    '';
  };

  home.packages = [ telega-launcher ];

  xdg.desktopEntries.telega-launcher = {
    name = "Telega Launcher";
    exec = "${telega-launcher}/bin/telega-launcher";
    terminal = false;
    mimeType = [ "x-scheme-handler/tg" ];
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "x-scheme-handler/tg" = [ "telega-launcher.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
      "x-scheme-handler/about" = [ "firefox.desktop" ];
      "x-scheme-handler/unknown" = [ "firefox.desktop" ];
    };
  };

  programs.wpaperd = {
    enable = true;
    settings = {
      default = {
        duration = "1d";
        mode = "center";
        sorting = "random";
      };

      any.path = ../wallpapers;
    };
  };

  services.swayidle = let cmd = "swaylock -fFkl -c #008080"; in {
    enable = true;
    events = [
      { event = "before-sleep"; command = cmd; }
      { event = "lock"; command = cmd; }
    ];
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

  # Previous default version, see https://github.com/nix-community/home-manager/blob/master/docs/release-notes/rl-2211.adoc
  home.stateVersion = "18.09";
}
