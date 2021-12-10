{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.irccat;
  description = "irccat - forward messages to IRC";

  # irccat expects to read its configuration from the *current
  # directory*, and its configuration contains secrets.
  #
  # To make this work we construct the JSON configuration file and
  # then recursively merge it with an on-disk secret using jq on
  # service launch.
  configJson = pkgs.writeText "irccat.json" (builtins.toJSON cfg.config);
  configMerge = pkgs.writeShellScript "merge-irccat-config" ''
    if [ ! -f "$CREDENTIALS_DIRECTORY/secrets" ]; then
      echo "irccat secrets file is missing"
      exit 1
    fi

    # jq's * is the recursive merge operator
    ${pkgs.jq}/bin/jq -s '.[0] * .[1]' ${configJson} "$CREDENTIALS_DIRECTORY/secrets" \
      > /var/lib/irccat/irccat.json
  '';
in {
  options.services.depot.irccat = {
    enable = lib.mkEnableOption description;

    config = lib.mkOption {
      type = lib.types.attrs; # varying value types
      description = "Configuration structure (unchecked!)";
    };

    secretsFile = lib.mkOption {
      type = lib.types.str;
      description = "Path to the secrets file to be merged";
      default = "/run/agenix/irccat";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.irccat = {
      inherit description;
      preStart = "${configMerge}";
      script = "${depot.third_party.irccat}/bin/irccat";
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "irccat";
        WorkingDirectory = "/var/lib/irccat";
        LoadCredential = "secrets:${cfg.secretsFile}"
        Restart = "always";
      };
    };
  };
}
