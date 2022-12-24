# public-inbox configuration for depot@tvl.su
#
# The account itself is a Yandex 360 account in the tvl.su organisation, which
# is accessed via IMAP. Yandex takes care of spam filtering for us, so there is
# no particular SpamAssassin or other configuration.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.inbox;
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
      };

      settings.coderepo.depot = {
        dir = cfg.depotPath;
        cgitUrl = "https://code.tvl.fyi";
      };
    };
  };
}
