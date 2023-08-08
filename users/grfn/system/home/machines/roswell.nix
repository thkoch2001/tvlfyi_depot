{ pkgs, lib, config, ... }:

let
  inherit (builtins) pathExists;
in

{
  imports = [
    ../platforms/linux.nix
    ../modules/shell.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/vim.nix
    ../modules/development/readyset.nix
    ../modules/tmux.nix
  ] ++ (lib.optional (pathExists ../modules/private.nix) ../modules/private.nix);

  home.packages = with pkgs; [
    # System utilities
    bat
    htop
    killall
    bind
    zip
    unzip
    tree
    nmap
    bc
    pv

    # Security
    gnupg
    keybase
    openssl

    # Nix things
    nixfmt
    nix-prefetch-github
    cachix

    # TODO(aspen): remove `hiPrio` once the ZSH completions don't conflict with HM anymore
    (lib.hiPrio nixpkgs-review)

    # ReadySet stuff
    nodejs
    mysql80

    (writeShellScriptBin "xdg-open" "echo xdg-open: \"$@\"")
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
