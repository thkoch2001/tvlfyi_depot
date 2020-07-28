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

  stunnelConfig = builtins.toFile "stunnel-freenode" ''
    foreground = yes

    [freenode]
    accept = 127.0.0.1:36633
    connect = irc.freenode.net:6697
  '';
in {
  options.services.depot.tvlbot.enable = mkEnableOption "TVL IRC bot";

  config = mkIf cfg.enable {
    # ii does not have built-in TLS support, but we can work around that.
    systemd.services.tvlbot-stunnel = {
      description = "TLS proxy for tvlbot's ii";
      script = "${pkgs.stunnel}/bin/stunnel ${stunnelConfig}";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
      };
    };

    systemd.services.tvlbot = {
      description = "tvlbot via ii";
      script = "${pkgs.ii}/bin/ii -s localhost -p 36633 -n tvlbot -f 'TVL Bot' -i %s/tvlbot";

      serviceConfig = {
        Restart = "always";
        User = "tvlbot";
        Group = "tvlbot";
        StateDirectory = "tvlbot";
        EnvironmentFile = "/etc/secrets/tvlbot";
      };

      postStart = ''
        # wait for connection to be up
        tail -f %S/tvlbot/out | sed '/End of \/MOTD command/ q'

        # auth to nickserv
        echo '/j nickserv' > %S/tvlbot/localhost/in
        echo 'identify $TVLBOT_NICKSERV' > %S/tvlbot/localhost/nickserv/in

        # join ##tvl
        echo '/j ##tvl' > %S/tvlbot/localhost/in
      '';
    };

    users = {
      users.tvlbot.isNormalUser = false;
      groups.tvlbot = {};
    };
  };
}
