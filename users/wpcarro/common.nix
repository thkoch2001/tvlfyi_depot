{ depot, pkgs, ... }:

let
  inherit (depot.users) wpcarro;
in {
  programs = {
    fish.enable = true;

    ssh = {
      startAgent = true;
      extraConfig = ''
        AddKeysToAgent yes
      '';
    };

    git = {
      enable = true;
      config = {
        user.name = "William Carroll";
        user.email = "wpcarro@gmail.com";
      };
    };
  };

  services = {
    locate.enable = true;

    depot.automatic-gc = {
      enable = true;
      interval = "1 hour";
      diskThreshold = 16; # GiB
      maxFreed = 10; # GiB
      preserveGenerations = "14d";
    };
  };

  # Command-line tools I commonly used and want available on most (or all) of my
  # machines.
  shell-utils = with pkgs; [
    bat
    dig
    direnv
    diskus
    emacs
    exa
    fd
    fzf
    git
    jq
    mkpasswd
    nmap
    python3
    ripgrep
    tldr
    tokei
    tree
    vim
    whois
    # TODO(wpcarro): Debug this failing build.
    # wpcarro.tools.simple_vim
    xclip
  ];
}
