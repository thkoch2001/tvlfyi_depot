# Module that configures CLBot, our Gerrit->IRC info bridge.
{ depot, config, lib, pkgs, ... }:

let
  inherit (builtins) attrValues concatStringsSep mapAttrs readFile;
  inherit (pkgs) runCommandNoCC;

  inherit (lib)
    listToAttrs
    mkEnableOption
    mkIf
    mkOption
    removeSuffix
    types;

  description = "Bot to forward CL notifications";
  cfg = config.services.depot.clbot;

  mkFlags = flags:
    concatStringsSep " "
      (attrValues (mapAttrs (key: value: "-${key} \"${toString value}\"") flags));

  # Escapes a unit name for use in systemd
  systemdEscape = name: removeSuffix "\n" (readFile (runCommandNoCC "unit-name" { } ''
    ${pkgs.systemd}/bin/systemd-escape '${name}' >> $out
  ''));

  mkUnit = flags: channel: {
    name = "clbot-${systemdEscape channel}";
    value = {
      description = "${description} to ${channel}";
      wantedBy = [ "multi-user.target" ];

      script = "${depot.fun.clbot}/bin/clbot ${mkFlags (cfg.flags // {
        irc_channel = channel;
      })} -alsologtostderr";

      serviceConfig = {
        User = "clbot";
        EnvironmentFile = cfg.secretsFile;
        Restart = "always";
      };
    };
  };
in
{
  options.services.depot.clbot = {
    enable = mkEnableOption description;

    flags = mkOption {
      type = types.attrsOf types.str;
      description = "Key value pairs for command line flags";
    };

    channels = mkOption {
      type = with types; listOf str;
      description = "Channels in which to post (generates one unit per channel)";
    };

    secretsFile = mkOption {
      type = types.str;
      description = "EnvironmentFile from which to load secrets";
      default = "/run/agenix/clbot";
    };
  };

  config = mkIf cfg.enable {
    # This does not use DynamicUser because we need to make some files
    # (notably the SSH private key) readable by this user outside of
    # the module.
    users = {
      groups.clbot = { };

      users.clbot = {
        group = "clbot";
        isSystemUser = true;
      };
    };

    systemd.services = listToAttrs (map (mkUnit cfg.flags) cfg.channels);
  };
}
