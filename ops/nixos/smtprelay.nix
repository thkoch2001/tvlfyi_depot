# NixOS module for configuring the simple SMTP relay.
{ pkgs, config, lib, ... }:

let
  inherit (builtins) attrValues mapAttrs;
  inherit (lib)
    concatStringsSep
    mkEnableOption
    mkOption
    types
;

  cfg = config.services.depot.smtprelay;
  description = "Simple SMTP relay";

  # Configuration values that are always overridden. In particular,
  # `config` is specified to always load $StateDirectory/secure.config
  # (so that passwords can be loaded from there) and logging is pinned
  # to stdout for journald compatibility.
  overrideArgs = {
    logfile = "";
    config = "/var/lib/smtprelay/secure.config";
  };

  # Creates the command line argument string for the service.
  prepareArgs = args:
    concatStringsSep " "
      (attrValues (mapAttrs (key: value: "-${key} '${toString value}'")
                            (args // overrideArgs)));
in {
  options.services.depot.smtprelay = {
    enable = mkEnableOption description;
    args = mkOption {
      type = types.attrsOf types.str;
      description = "Key value pairs for command line arguments";
    };
  };

  config = {
    systemd.services.smtprelay = {
      inherit description;
      script = "${config.depot.third_party.smtprelay}/bin/smtprelay ${prepareArgs cfg.args}";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        StateDirectory = "smtprelay";
        DynamicUser = true;
      };
    };
  };
}
