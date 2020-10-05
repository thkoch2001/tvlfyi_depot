{ config, lib, pkgs, ... }:

# Everything in here needs to work on linux or darwin

{
  imports = [
    ../modules/shell.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/vim.nix
    ../modules/tarsnap.nix
    ../modules/twitter.nix
    ../modules/lib/cloneRepo.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs.password-store.enable = true;

  grfn.impure.clonedRepos.passwordStore = {
    github = "glittershark/pass";
    path = ".local/share/password-store";
  };

  home.packages = with pkgs; [
    # System utilities
    bat
    htop
    killall
    bind
    zip unzip
    tree
    ncat
    bc
    pv

    # Security
    gnupg
    keybase
    openssl

    # Nix things
    nixfmt
    nix-prefetch-github
    nix-review
    cachix
  ];

  programs.ssh = {
    enable = true;

    matchBlocks = {
      "home" = {
        host = "home.gws.fyi";
        forwardAgent = true;
      };

      "dobharchu" = {
        host = "dobharchu";
        hostname = "172.16.0.4";
        forwardAgent = true;
        user = "griffin";
      };

      "mugwump" = {
        host = "mugwump";
        hostname = "172.16.0.5";
        forwardAgent = true;
      };
    };
  };
}
