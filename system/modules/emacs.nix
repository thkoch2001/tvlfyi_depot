{ config, pkgs, lib, ... }:
with lib;
{
  options.programs.emacs.useGit = mkOption {
    description = "Use emacs from git";
    type = types.bool;
    default = false;
  };

  config = {
    nixpkgs.overlays = if config.programs.emacs.useGit then [] else [
      (import (builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz))
    ];

    environment.systemPackages = with pkgs; [
      (if config.programs.emacs.useGit then emacsGit else emacs)
      ripgrep
      coreutils
      fd
      clang
    ];
  };
}
