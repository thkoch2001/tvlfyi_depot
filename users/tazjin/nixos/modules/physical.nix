# Default configuration settings for physical machines that I use.
{ lib, pkgs, depot, ... }:

let
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in
{
  # Install all the default software.
  environment.systemPackages =
    # programs from the depot
    (with depot; [
      users.tazjin.screenLock
      users.tazjin.emacs
      third_party.agenix.cli
      third_party.josh
    ]) ++

    # programs from nixpkgs
    (with pkgs; [
      amber
      bat
      curl
      ddcutil
      direnv
      dnsutils
      electrum
      emacs-unstable # emacsclient
      exa
      fd
      file
      gdb
      gh
      git
      gnupg
      gtk3 # for gtk-launch
      htop
      hyperfine
      iftop
      imagemagick
      jq
      lieer
      maim
      man-pages
      moreutils
      mosh
      msmtp
      mullvad-vpn
      networkmanagerapplet
      nix-prefetch-github
      nmap
      notmuch
      openssh
      openssl
      pass-otp
      pavucontrol
      pinentry
      pinentry-emacs
      pulseaudio # for pactl
      pwgen
      quasselClient
      rink
      ripgrep
      rust-analyzer
      rustup
      screen
      thunderbird
      tig
      tokei
      tree
      unzip
      vlc
      volumeicon
      whois
      xclip
      xsecurelock
      zoxide
    ]);

  # Run services & configure programs for all machines.
  services.fwupd.enable = true;

  # Disable the broken NetworkManager-wait-online.service
  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;

  programs = {
    fish.enable = true;
    mosh.enable = true;
    ssh.startAgent = true;
  };
}
