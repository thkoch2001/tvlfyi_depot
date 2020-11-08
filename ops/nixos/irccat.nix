{ config, lib, pkgs, ... }:

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
    if [ ! -f "/etc/secrets/irccat.json" ]; then
      echo "irccat secrets file is missing"
      exit 1
    fi

    # jq's * is the recursive merge operator
    ${pkgs.jq}/bin/jq -s '.[0] * .[1]' ${configJson} /etc/secrets/irccat.json \
      > /var/lib/irccat/irccat.json
cat  '';
in {
  options.services.depot.irccat = {
    enable = lib.mkEnableOption description;

    config = lib.mkOption {
      type = lib.types.attrs; # varying value types
      description = "Configuration structure (unchecked!)";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.irccat = {
      inherit description;
      preStart = "${configMerge}";
      script = "${config.depot.third_party.irccat}/bin/irccat";

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "irccat";
        WorkingDirectory = "/var/lib/irccat";
        Restart = "always";
      };
    };
  };
}
