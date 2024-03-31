{ lib, pkgs, config, ... }:

with lib;

let

  # from home-manager/modules/services/lieer.nix
  escapeUnitName = name:
    let
      good = upperChars ++ lowerChars ++ stringToCharacters "0123456789-_";
      subst = c: if any (x: x == c) good then c else "-";
    in
    stringAsChars subst name;

  accounts = {
    personal = {
      primary = true;
      address = "root@gws.fyi";
      aliases = [ "aspen@gws.fyi" "aspen@gws.fyi" ];
      passEntry = "root-gws-msmtp";
    };
  };

in
{
  # 2022-09-26: workaround for home-manager defaulting to removed pkgs.gmailieer
  # attribute, can likely be removed soon
  programs.lieer.package = pkgs.lieer;

  programs.lieer.enable = true;
  programs.notmuch.enable = true;
  services.lieer.enable = true;
  programs.msmtp.enable = true;

  home.packages = with pkgs; [
    mu
    msmtp
    config.lib.depot.users.aspen.pkgs.notmuch-extract-patch
  ];

  systemd.user.services = mapAttrs'
    (name: account: {
      name = escapeUnitName "lieer-${name}";
      value.Service = {
        ExecStart = mkForce "${pkgs.writeShellScript "sync-${name}" ''
        ${pkgs.lieer}/bin/gmi sync --path ~/mail/${name}
      ''}";
        Environment =
          "NOTMUCH_CONFIG=${config.home.sessionVariables.NOTMUCH_CONFIG}";
      };

    })
    accounts;

  accounts.email.maildirBasePath = "mail";
  accounts.email.accounts = mapAttrs
    (_:
      params@{ passEntry, ... }:
      {
        realName = "Aspen Smith";
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
      } // builtins.removeAttrs params [ "passEntry" ])
    accounts;
}
