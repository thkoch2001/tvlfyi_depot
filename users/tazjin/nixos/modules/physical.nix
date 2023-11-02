# Default configuration settings for physical machines that I use.
{ lib, pkgs, config, depot, ... }:

let
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in
{
  options = with lib; {
    tazjin.emacs = mkOption {
      type = types.package;
      default = depot.users.tazjin.emacs;
      description = ''
        Derivation with my Emacs package, with configuration included.
      '';
    };
  };

  config = {
    # Install all the default software.
    environment.systemPackages =
      # programs from the depot
      (with depot; [
        users.tazjin.screenLock
        users.tazjin.chase-geese
        config.tazjin.emacs
        third_party.agenix.cli
        third_party.josh
      ]) ++

      # programs from nixpkgs
      (with pkgs; [
        (aspellWithDicts (d: [ d.ru ]))
        amber
        bat
        curl
        ddcutil
        direnv
        dnsutils
        electrum
        config.tazjin.emacs.emacs # emacsclient
        expect
        fd
        file
        gdb
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
        rustup
        screen
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
  };
}
