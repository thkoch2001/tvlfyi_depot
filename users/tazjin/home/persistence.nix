# Persistence configuration for machines with throw-away setups.

{ depot, pkgs, ... }: # readTree
{ config, lib, ... }: # home-manager

{
  imports = [ (depot.third_party.sources.impermanence + "/home-manager.nix") ];

  home.persistence."/persist/tazjin/home" = {
    allowOther = true;

    directories = [
      ".cargo"
      ".config/audacity"
      ".config/chromium"
      ".config/google-chrome"
      ".config/quassel-irc.org"
      ".config/syncthing"
      ".config/unity3d"
      ".electrum"
      ".gnupg"
      ".local/share/audacity"
      ".local/share/direnv"
      ".local/share/fish"
      ".local/share/keyrings"
      ".local/share/zoxide"
      ".mozilla/firefox"
      ".password-store"
      ".rustup"
      ".ssh"
      ".steam"
      ".telega"
      ".thunderbird"
      "go"
      "mail"
    ];

    files = [
      ".notmuch-config"
    ];
  };
}
