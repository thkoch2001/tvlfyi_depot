# urbint-only dev stuff
{ config, lib, pkgs, ... }:

let

  yarn2nix = (import (pkgs.fetchFromGitHub {
    owner = "moretea";
    repo = "yarn2nix";
    rev = "9e7279edde2a4e0f5ec04c53f5cd64440a27a1ae";
    sha256 = "0zz2lrwn3y3rb8gzaiwxgz02dvy3s552zc70zvfqc0zh5dhydgn7";
  }) { inherit pkgs; }).yarn2nix;

in

{
  home.packages = with pkgs; [
    yarn2nix
    python36
    python36Packages.ipython
  ];

  programs.zsh = {
    shellAliases = {
      ipy = "ipython";
      amerge = "alembic merge heads";
    };

    initExtra = ''
      # Alembic {{{
      function aup() {
        alembic upgrade ''${1:-head}
      }

      function adown() {
        alembic downgrade ''${1:--1}
      }
    '';
  };

  programs.git = {
    extraConfig.filter.black100to80 =
      let inherit (pkgs.python36Packages) black; in {
        clean = "${black}/bin/black --target-version py36 -l 100 -";
        smudge = "${black}/bin/black --target-version py36 -l 80 -";
      };


    includes = [{
      condition = "gitdir:~/code/urb/";
      contents = {
        user.email = "grfn@urbint.com";
      };
    }];
  };

  home.file.".ipython/profile_default/ipython_config.py".text = ''
    c.InteractiveShellApp.exec_lines = ['%autoreload 2']
    c.InteractiveShellApp.extensions = ['autoreload']
    c.TerminalInteractiveShell.editing_mode = 'vi'
  '';
}
