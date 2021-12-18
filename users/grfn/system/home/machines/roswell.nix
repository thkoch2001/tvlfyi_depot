{ pkgs, lib, config, ... }:

let
  laptopKeyboardId = "5";
in

{
  imports = [
    ../platforms/linux.nix
    ../modules/shell.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/vim.nix
  ];

  home.packages = with pkgs; [
    # System utilities
    bat
    htop
    killall
    bind
    zip
    unzip
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

  programs.password-store.enable = true;

  programs.home-manager.enable = true;
  home.stateVersion = "20.03";

  xsession.enable = lib.mkForce false;

  services.lorri.enable = true;

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
