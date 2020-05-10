{ config, pkgs, lib, ... }:
with lib;
{
  options = {
    programs.emacs.useGit = mkOption {
      description = "Use emacs from git";
      type = types.bool;
      default = false;
    };

    programs.emacs.useUnstable = mkOption {
      description = "Use emacs unstable";
      type = types.bool;
      default = false;
    };
  };

  config = {
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/54afb061bdd12c61bbfcc13bad98b7a3aab7d8d3.tar.gz";
        sha256 = "0hrbg65d5h0cb0nky7a46md7vlvhajq1hf0328l2f7ln9hznqz6j";
      }))
    ];

    environment.systemPackages = with pkgs; [
      (if config.programs.emacs.useGit
       then emacsGit
       else
         if config.programs.emacs.useUnstable
         then emacsUnstable
         else emacs)
      ripgrep
      coreutils
      fd
      clang
    ];

    programs.emacs.useUnstable = true;
  };
}
