{ config, lib, pkgs, ... }:

{
  imports = [
    ./development/kube.nix
  ];

  home.packages = with pkgs; [
    jq
    gitAndTools.hub
    gitAndTools.tig
    shellcheck
    httpie
    entr
    gnumake
    inetutils
    (import (pkgs.fetchFromGitHub {
      owner = "moretea";
      repo = "yarn2nix";
      rev = "9e7279edde2a4e0f5ec04c53f5cd64440a27a1ae";
      sha256 = "0zz2lrwn3y3rb8gzaiwxgz02dvy3s552zc70zvfqc0zh5dhydgn7";
    }) { inherit pkgs; }).yarn2nix
    julia
  ];

  programs.git = {
    enable = true;
    # bah, this doesn't work
    # package = pkgs.git.override {
    #   sendEmailSupport = true;
    # };
    userEmail = "root@gws.fyi";
    userName  = "Griffin Smith";
    ignores = [
      "*.sw*"
      ".classpath"
      ".project"
      ".settings/"
      ".dir-locals.el"
      ".stack-work-profiling"
      ".projectile"
    ];
    extraConfig = {
      github.user = "glittershark";
      merge.conflictstyle = "diff3";
    };
  };

  home.file.".psqlrc".text = ''
    \set QUIET 1
    \timing
    \set ON_ERROR_ROLLBACK interactive
    \set VERBOSITY verbose
    \x auto
    \set PROMPT1 '%[%033[1m%]%M/%/%R%[%033[0m%]%# '
    \set PROMPT2 '...%# '
    \set HISTFILE ~/.psql_history- :DBNAME
    \set HISTCONTROL ignoredups
    \pset null [null]
    \unset QUIET
  '';

  home.file.".ipython/profile_default/ipython_config.py".text = ''
    c.InteractiveShellApp.exec_lines = ['%autoreload 2']
    c.InteractiveShellApp.extensions = ['autoreload']
    c.TerminalInteractiveShell.editing_mode = 'vi'
  '';

  programs.readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
    '';
  };
}
