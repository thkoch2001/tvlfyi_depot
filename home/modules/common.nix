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

  urbint.projectPath = "code/urb";

  home.packages = with pkgs; [
    # System utilities
    bat
    htop
    killall
    bind
    zip unzip
    tree
    ncat

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
}
