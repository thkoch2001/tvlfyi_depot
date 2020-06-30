# Module that configures CLBot, our Gerrit->IRC info bridge.
{ config, lib, pkgs, ... }:

let
  inherit (builtins) concatStringsSep attrValues mapAttrs;
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types;

  description = "CLBot forwards Gerrit notifications to IRC";
  cfg = config.services.depot.clbot;

  mkFlags = flags:
    concatStringsSep " "
      (attrValues (mapAttrs (key: value: "-${key} \"${toString value}\"") flags));
in {
  options.services.depot.clbot = {
    enable = mkEnableOption description;
    flags = mkOption {
      type = types.attrsOf types.str;
      description = "Key value pairs for command line flags";
    };
  };

  config = mkIf cfg.enable {
    # This does not use DynamicUser because we need to make some files
    # (notably the SSH private key) readable by this user outside of
    # the module.
    users = {
      groups.clbot = {};

      users.clbot = {
        group = "clbot";
        isNormalUser = false;
      };
    };

    systemd.services.clbot = {
      inherit description;
      script = "${config.depot.fun.clbot}/bin/clbot ${mkFlags cfg.flags} -alsologtostderr";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        User = "clbot";
        EnvironmentFile = "/etc/secrets/clbot";
        Restart = "always";
      };
    };
  };
}
