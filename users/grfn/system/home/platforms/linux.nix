{ config, pkgs, ... }:

let

  depot = config.lib.depot;

in

{
  imports = [
    ../modules/alacritty.nix
    ../modules/alsi.nix
    ../modules/development.nix
    ../modules/emacs.nix
    ../modules/email.nix
    ../modules/firefox.nix
    ../modules/games.nix
    ../modules/shell.nix
    ../modules/tarsnap.nix
    ../modules/vim.nix
  ];

  xsession.enable = true;

  home.packages = with pkgs; [
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
    depot.tools.nsfv-setup
    gimp # TODO(grfn): use glimpse once it build again

    # System utilities
    powertop
    usbutils
    pciutils
    gdmap
    lsof
    tree
    nmap
    iftop

    # Security
    gnupg
    keybase
    openssl
    yubikey-manager
    # TODO(grfn): lagging behind yubikey-manager and doesn't support cryptography >= 39
    # yubikey-manager-qt

    # Spotify...etc
    spotify
    playerctl
  ];

  services.redshift = {
    enable = true;
    provider = "geoclue2";
  };

  services.pasystray.enable = true;

  services.gpg-agent = {
    enable = true;
  };

  programs.zsh.initExtra = ''
    [[ ! $IN_NIX_SHELL && "$TERM" != "dumb" ]] && alsi -l
  '';

  services.lorri.enable = true;

  services.dropbox = {
    enable = true;
  };
}
