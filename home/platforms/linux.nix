{ config, pkgs, ... }:

{
  imports = [
    ../modules/alacritty.nix
    ../modules/alsi.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/email.nix
    ../modules/firefox.nix
    ../modules/games.nix
    ../modules/obs.nix
    ../modules/i3.nix
    ../modules/shell.nix
    ../modules/tarsnap.nix
    ../modules/vim.nix

    ~/code/urb/urbos/home

    machine
  ];

  xsession.enable = true;

  home.packages = with pkgs; [
    (import (fetchTarball "https://github.com/ashkitten/nixpkgs/archive/init-glimpse.tar.gz") {}).glimpse

    # Desktop stuff
    arandr
    firefox
    feh
    chromium
    xclip
    xorg.xev
    picom
    peek
    signal-desktop
    apvlv # pdf viewer
    vlc
    irssi
    gnutls
    pandoc
    barrier

    # System utilities
    powertop
    usbutils
    pciutils
    gdmap
    lsof
    tree
    ncat

    # Security
    gnupg
    keybase
    openssl

    # Spotify...etc
    spotify
    playerctl
  ];

  nixpkgs.config.allowUnfree = true;

  programs.password-store.enable = true;

  services.redshift = {
    enable = true;
    provider = "geoclue2";
  };

  services.pasystray.enable = true;

  impure.clonedRepos.passwordStore = {
    github = "glittershark/pass";
    path = ".local/share/password-store";
  };

  urbint.projectPath = "code/urb";

  services.gpg-agent = {
    enable = true;
  };

  gtk = {
    enable = true;
    gtk3.bookmarks = [
      "file:///home/grfn/code"
    ];
  };

  programs.tarsnap = {
    enable = true;
    keyfile = "/home/grfn/.private/tarsnap.key";
    printStats = true;
    humanizeNumbers = true;
  };

  programs.zsh.initExtra = ''
    [[ ! $IN_NIX_SHELL ]] && alsi -l
  '';
}
