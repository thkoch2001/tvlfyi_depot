# tvlbot is an instance of ii[0], connected to the TVL channel.
#
# it can be used by arbitrary programs on the host to send messages to
# the channel, making it very easy to provide integrations for logging
# events to the channel and other stuff, like such as that.

{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf
    types;

  cfg = config.services.depot.tvlbot;

  stunnelConfig = pkgs.writeText "stunnel-freenode" ''
    foreground = yes
    CAfile = ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt

    [freenode]
    client = yes
    accept = 127.0.0.1:36633
    connect = irc.freenode.net:6697
    verifyChain = yes
  '';
in {
  options.services.depot.tvlbot.enable = mkEnableOption "TVL IRC bot";

  config = mkIf cfg.enable {
    # ii does not have built-in TLS support, but we can work around that.
    systemd.services.tvlbot-stunnel = {
      description = "TLS proxy for tvlbot's ii";
      script = "${pkgs.stunnel}/bin/stunnel ${stunnelConfig}";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
      };
    };

    systemd.services.tvlbot = {
      description = "tvlbot via ii";
      script = "${pkgs.ii}/bin/ii -s 127.0.0.1 -p 36633 -n tvlbot -k $TVLBOT_NICKSERV -f 'TVL Bot' -i /var/lib/tvlbot";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        User = "tvlbot";
        Group = "tvlbot";
        StateDirectory = "tvlbot";
        EnvironmentFile = "/etc/secrets/tvlbot";
      };

      postStart = ''
        set -ue
        IRCDIR=/var/lib/tvlbot/127.0.0.1

        # wait for connection to be up
        sleep 5 # give it a few seconds to settle first
        tail -f  $IRCDIR/out | sed '/End of \/MOTD command/ q'

        # join ##tvl
        echo '/j ##tvl' > $IRCDIR/in
      '';
    };

    users = {
      users.tvlbot.isNormalUser = false;
      groups.tvlbot = {};
    };
  };
}
