{ lib, pkgs, ... }:

with lib;

let

  # from home-manager/modules/services/lieer.nix
  escapeUnitName = name:
    let
      good = upperChars ++ lowerChars ++ stringToCharacters "0123456789-_";
      subst = c: if any (x: x == c) good then c else "-";
    in stringAsChars subst name;

  accounts = {
    personal = {
      primary = true;
      address = "root@gws.fyi";
      aliases = [ "grfn@gws.fyi" ];
      passEntry = "root-gws-msmtp";
    };
  };

in {
  programs.lieer.enable = true;
  programs.notmuch.enable = true;
  services.lieer.enable = true;
  programs.msmtp.enable = true;

  home.packages = with pkgs; [
    mu
    msmtp
  ];

  # nixpkgs.overlays = [(self: super: {
  #   notifymuch = self.python3Packages.callPackage ../../pkgs/notifymuch.nix {};
  # })];

  systemd.user.services = mapAttrs' (name: account: {
    name = escapeUnitName "lieer-${name}";
    value.Service.ExecStart = mkForce "${pkgs.writeShellScript "sync-${name}" ''
      ${pkgs.gmailieer}/bin/gmi sync
    ''}";
    # ${pkgs.notifymuch}/bin/notifymuch
  }) accounts;

  # xdg.configFile."notifymuch/notifymuch.cfg".text = generators.toINI {} {
  #   notifymuch = {
  #     query = "is:unread and is:important";
  #     mail_client = "";
  #     recency_interval_hours = "48";
  #     hidden_tags = "inbox unread attachment replied sent encrypted signed";
  #   };
  # };

  accounts.email.maildirBasePath = "mail";
  accounts.email.accounts = mapAttrs (_: params@{ passEntry, ... }: {
    realName = "Griffin Smith";
    passwordCommand = "pass ${passEntry}";

    flavor = "gmail.com";

    imapnotify = {
      enable = true;
      boxes = [ "Inbox" ];
    };

    gpg = {
      key = "0F11A989879E8BBBFDC1E23644EF5B5E861C09A7";
      signByDefault = true;
    };

    notmuch.enable = true;
    lieer = {
      enable = true;
      sync = {
        enable = true;
        frequency = "*:*";
      };
    };
    msmtp.enable = true;
  } // builtins.removeAttrs params ["passEntry"]) accounts;
}
