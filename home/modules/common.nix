{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/shell.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/vim.nix
    ../modules/tarsnap.nix
  ];

  nixpkgs.config.allowUnfree = true;
  programs.password-store.enable = true;

  impure.clonedRepos.passwordStore = {
    github = "glittershark/pass";
    path = ".local/share/password-store";
  };

  home.packages = with pkgs; [
    htop
    killall
    bind
    zip unzip
    tree
    ncat

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
