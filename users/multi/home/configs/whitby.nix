{ depot, pkgs, ... }:

let
  nixpkgs = import pkgs.nixpkgsSrc {};
  localpkgs = depot.users.multi.pkgs;

  # use the depot checkout in my home directory, to make hacking around easier,
  # instead of the depot checkout sitting in the nix stores
  depotPath = "/home/multi/depot";
in

{
  programs = {
    home-manager = {
      enable = true;
      path = toString nixpkgs.home-manager.src;
    };

    bash = {
      enable = true;
      initExtra = ''
        PS1="[\\u@\\h:\\w"
        if [[ -n "$IN_NIX_SHELL" ]]; then
            PS1="$PS1 (nix-shell)]\\\$ "
        else
            PS1="$PS1]\\\$ "
        fi

        nix-shell() {
            local comarg=0
            for i in "$@"; do
                [[ "$i" == "--command" ]] && comarg=1
            done

            if (( commarg == 0 )); then
                command nix-shell --command bash "$@"
            else
                command nix-shell "$@"
            fi
        }

        _Z_CMD=d
        source ~/.z.sh
      '';
    };

    readline = {
      enable = true;
      bindings = {
        "\\e[5~" = "history-search-backward";
        "\\e[6~" = "history-search-forward";
        "\\C-w" = "\"\\e\\C-h\"";
      };
      includeSystemConfig = false;
      variables = {
        expand-tilde = true;
        colored-stats = true;
        page-completions = false;
        menu-complete-display-prefix = true;
        colored-completion-prefix = true;
        completion-query-items = 0;
        completion-ignore-case = true;
        revert-all-at-newline = true;
        show-all-if-ambiguous = true;
        skip-completed-text = true;
      };
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
      "nixpkgs=${pkgs.nixpkgsSrc}:" +
      "depot=${depotPath}";
    HOME_MANAGER_CONFIG = "${depotPath}/users/multi/home/home-manager.nix";
    EDITOR = "vim";
  };

  home.packages = [
    nixpkgs.lsof
    nixpkgs.strace
    nixpkgs.file
    nixpkgs.pciutils
    localpkgs.htop
  ];

  home.file = {
    z = {
      source = builtins.fetchurl "https://raw.githubusercontent.com/rupa/z/9f76454b32c0007f20b0eae46d55d7a1488c9df9/z.sh";
      target = ".z.sh";
    };
  };

  home.stateVersion = "20.03";
}
