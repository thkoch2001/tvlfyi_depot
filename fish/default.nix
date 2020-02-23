{ pkgs, ... }:

# TODO: Is it appropriate to put programming language dependencies here? Should
# I have a bin dependency for every fish `abbr` and `alias` that I use? What
# makes the most sense?

# TODO: Some of the abbreviations / aliases depend on binaries and some depend
# on files (e.g. ~/.config/nvim/init.vim). How should I handle the file
# dependencies?

# TODO: Support symlinking config.fish to ~/.config/fish/config.fish using Nix.

let
  fishBinPath = pkgs.lib.strings.makeBinPath (with pkgs; [
    # TODO: Support fasd instead of autojump.
    # fasd
    direnv
    autojump
    fzf
    fd
    xclip
    bat
    neovim
    ripgrep
    sdcv
    exa
    pass
    networkmanager
    google-chrome
    docker
    elixir
    clojure
    gnupg
    git
    tmux
    # This is not that same as `hub`.
    # git-hub
    mercurial
    stack
    kubernetes
    circleci-cli
    nix # Really?
    apt # Really?
    pacman # Really?
  ]);
# TODO: It's difficult to test if the `--init-command` is working since fish
# persists functions, abbreviations, aliases between sessions so it's easy to
# get tricked by false-positives.
in pkgs.writeShellScriptBin "wpcarros-fish" ''
  export PATH="${fishBinPath}:$PATH"
  exec ${pkgs.fish}/bin/fish --init-command 'source ${ ./functions.fish }'
''
