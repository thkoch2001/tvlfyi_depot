{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    t
  ];

  home.sessionVariables = {
    TWITTER_WHOAMI = "glittershark1";
  };

  programs.zsh.shellAliases = {
    "mytl" = "t tl $TWITTER_WHOAMI";
  };
}
