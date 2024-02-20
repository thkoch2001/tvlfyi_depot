{ config, lib, pkgs, ... }:

# Everything in here needs to work on linux or darwin, with or without a desktop
# environment

{
  imports = [
    ../modules/shell.nix
    # ../modules/development.nix
    ../modules/emacs.nix
    ../modules/vim.nix
    ../modules/tarsnap.nix
    ../modules/twitter.nix
    ../modules/lib/cloneRepo.nix
  ];

  home.username = "grfn";
  home.homeDirectory = "/home/grfn";

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
    nixpkgs-review
    cachix
    (writeShellScriptBin "rebuild-mugwump" ''
      set -eo pipefail
      cd ~/code/depot
      nix build -f . users.aspen.system.system.mugwumpSystem -o /tmp/mugwump
      nix copy -f . users.aspen.system.system.mugwumpSystem \
        --to ssh://mugwump
      system=$(readlink -ef /tmp/mugwump)
      ssh mugwump sudo nix-env -p /nix/var/nix/profiles/system --set $system
      ssh mugwump sudo $system/bin/switch-to-configuration switch
      rm /tmp/mugwump
    '')
    (writeShellScriptBin "rebuild-roswell" ''
      set -eo pipefail
      cd ~/code/depot
      nix build -f . users.aspen.system.system.roswellSystem -o /tmp/roswell
      nix copy -f . users.aspen.system.system.roswellSystem \
        --to ssh://roswell
      system=$(readlink -ef /tmp/roswell)
      ssh roswell sudo nix-env -p /nix/var/nix/profiles/system --set $system
      ssh roswell sudo $system/bin/switch-to-configuration switch
      rm /tmp/roswell
    '')
    (writeShellScriptBin "rebuild-home" ''
      set -eo pipefail
      cd ~/code/depot
      nix build -f . users.aspen.system.home.$(hostname)Home -o /tmp/home
      /tmp/home/activate
    '')
  ];

  programs.ssh = {
    enable = true;

    matchBlocks = {
      "home" = {
        host = "home.gws.fyi";
        forwardAgent = true;
      };

      "cerberus" = {
        host = "cerberus";
        hostname = "172.16.0.3";
        forwardAgent = true;
        user = "griffin";
      };

      "mugwump" = {
        host = "mugwump";
        hostname = "172.16.0.5";
        forwardAgent = true;
      };

      "roswell" = {
        host = "roswell";
        forwardAgent = true;
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;
  };
}
