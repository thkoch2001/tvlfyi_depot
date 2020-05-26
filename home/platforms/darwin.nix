{ config, lib, pkgs, ... }:

with lib;

{
  home.packages = with pkgs; [
    coreutils
    gnupg
    pinentry_mac
  ];

  home.activation.linkApplications = lib.hm.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG \
      ~/.nix-profile/Applications/* ~/Applications/
  '';

  programs.zsh.initExtra = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH

    if [[ "$TERM" == "alacritty" ]]; then
      export TERM="xterm-256color"
    fi
  '';
}
