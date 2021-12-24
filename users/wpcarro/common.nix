{ pkgs, ... }:

{
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
    xclip
  ];
}
