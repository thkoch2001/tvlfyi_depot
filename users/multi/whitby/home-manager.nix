{ config ? throw "not a readTree target", pkgs, depot, ... }:

let
  hmPath = "$HOME/nix/home-manager";

in

{
  programs = {
    home-manager = { enable = true; path = hmPath; };

    bash = {
      enable = true;
      initExtra = ''
        bind '"\e[5~":history-search-backward'
        bind '"\e[6~":history-search-forward'

        PS1="[\\u@\\h:\\w]\\\$ "

        _Z_CMD=d
        source ~/.z.sh
      '';
    };

    tmux = {
      enable = true;
      terminal = "tmux-256color";
      escapeTime = 50;
      extraConfig = ''
        bind-key -n C-S-Left swap-window -t -1
        bind-key -n C-S-Right swap-window -t +1
      '';
    };

    vim = {
      enable = true;
      extraConfig = "set mouse=";
    };
  };

  home.sessionVariables = {
    NIX_PATH =
      "nixpkgs=$HOME/nix/nixpkgs:" +
      "home-manager=${hmPath}:" +
      "depot=$HOME/nix/depot:" +
      "/nix/var/nix/profiles/per-user/root/channels";
    HOME_MANAGER_CONFIG = <depot/users/multi/whitby/home-manager.nix>;
    EDITOR = "vim";
  };

  home.packages = [
    pkgs.lsof
    pkgs.strace
    pkgs.file
    pkgs.pciutils
  ] ++ (import ../pkgs { inherit pkgs; });

  home.file = {
    z = {
      source = builtins.fetchurl "https://raw.githubusercontent.com/rupa/z/master/z.sh";
      target = ".z.sh";
    };
  };


  home.stateVersion = "20.03";
}
