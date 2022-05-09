# Default configuration settings for physical machines that I use.
{ pkgs, depot, ... }:

{
  # Install all the default software.
  environment.systemPackages =
    # programs from the depot
    (with depot; [
      users.tazjin.screenLock
      users.tazjin.emacs
      third_party.agenix.cli
    ]) ++

    # programs from nixpkgs
    (with pkgs; [
      amber
      audacity
      bat
      curl
      ddcutil
      direnv
      dmd
      dnsutils
      electrum
      emacsNativeComp # emacsclient
      exa
      fd
      file
      firefox
      fractal
      gdb
      gh
      git
      gnupg
      google-chrome
      gtk3 # for gtk-launch
      htop
      hyperfine
      iftop
      imagemagick
      jq
      lieer
      man-pages
      mosh
      msmtp
      mullvad-vpn
      networkmanagerapplet
      nix-prefetch-github
      nmap
      notmuch
      openssh
      openssl
      paperlike-go
      pass
      pavucontrol
      pinentry
      pinentry-emacs
      pulseaudio # for pactl
      pwgen
      quasselClient
      rink
      ripgrep
      rustup
      screen
      scrot
      tig
      tokei
      tree
      unzip
      vlc
      whois
      xsecurelock
      zoxide
    ]);
}
