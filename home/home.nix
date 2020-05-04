{ config, pkgs, ... }:

let machine = ./machines/chupacabra.nix; in
{
  imports = [
    ./modules/alacritty.nix
    ./modules/alsi.nix
    ./modules/development.nix
    ./modules/emacs.nix
    ./modules/email.nix
    ./modules/firefox.nix
    ./modules/i3.nix
    ./modules/shell.nix
    ./modules/tarsnap.nix
    ./modules/vim.nix

    ./modules/lib/cloneRepo.nix

    ./modules/urbint

    machine
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  xsession.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "19.09";

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

    # System utilities
    htop
    powertop
    usbutils
    pciutils
    killall
    gdmap
    bind
    lsof
    zip
    tree
    ncat
    unzip

    # Security
    gnupg
    keybase
    openssl

    # Spotify...etc
    spotify
    playerctl

    # games
    crawl
    dwarf-fortress

    # Nix things
    nix-prefetch-github
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
}
