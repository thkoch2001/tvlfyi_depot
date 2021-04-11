{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    t
  ];

  home.sessionVariables = {
    TWITTER_WHOAMI = "glittershark1";
  };

  programs.zsh = {
    shellAliases = {
      "mytl" = "t tl $TWITTER_WHOAMI";
    };

    functions = {
      favelast = "t fave $(t tl -l $1 | head -n1 | cut -d' ' -f1)";
      rtlast = "t rt $(t tl -l $1 | head -n1 | cut -d' ' -f1)";
      tthread = "t reply $(t tl -l $TWITTER_WHOAMI | head -n1 | cut -d' ' -f1) $@";
    };
  };
}
