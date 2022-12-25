# public-inbox configuration for depot@tvl.su
#
# The account itself is a Yandex 360 account in the tvl.su organisation, which
# is accessed via IMAP. Yandex takes care of spam filtering for us, so there is
# no particular SpamAssassin or other configuration.
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.services.depot.inbox;

  imapConfig = pkgs.writeText "offlineimaprc" ''
    [general]
    accounts = depot

    [Account depot]
    localrepository = Local
    remoterepository = Remote

    [Repository Local]
    type = Maildir
    localfolders = /var/lib/public-inbox/depot-imap

    [Repository Remote]
    type = IMAP
    ssl = yes
    sslcacertfile = ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    remotehost = imap.yandex.ru
    remoteuser = depot@tvl.su
    remotepassfile = /var/run/agenix/depot-inbox-imap
  '';
in
{
  options.services.depot.inbox = with lib; {
    enable = mkEnableOption "Enable public-inbox for depot@tvl.su";

    depotPath = mkOption {
      description = "path to local depot replica";
      type = types.str;
      default = "/var/lib/depot";
    };
  };

  config = lib.mkIf cfg.enable {
    services.public-inbox = {
      enable = true;

      http.enable = true;
      http.port = 8053;
      # imap.enable = true;
      # nntp.enable = true;

      inboxes.depot = rec {
        address = [
          "depot@tvl.su" # primary address
          "depot@tazj.in" # legacy address
        ];

        description = "TVL depot development";
        coderepo = [ "depot" ];
        url = "https://inbox.tvl.su/depot";

        watch = [
          "maildir:/var/lib/public-inbox/depot-imap/"
        ];
      };

      settings.coderepo.depot = {
        dir = cfg.depotPath;
        cgitUrl = "https://code.tvl.fyi";
      };
    };

    age.secrets.depot-inbox-imap = {
      file = depot.ops.secrets."depot-inbox-imap.age";
      mode = "0440";
      group = config.users.groups."public-inbox".name;
    };

    systemd.services.offlineimap-depot = {
      description = "download mail for depot@tvl.su";
      wantedBy = [ "multi-user.target" ];
      startAt = "minutely";

      script = ''
        mkdir -p /var/lib/public-inbox/depot-imap
        ${pkgs.offlineimap}/bin/offlineimap -c ${imapConfig}
      '';

      serviceConfig = {
        Type = "oneshot";

        # Run in the same user context as public-inbox itself to avoid
        # permissions trouble.
        User = config.users.users."public-inbox".name;
        Group = config.users.groups."public-inbox".name;
      };
    };
  };
}
