# This module serves hound.
#
# https://github.com/hound-search/hound
{ pkgs, config, lib, ... }:

let
  cfg = config.services.depot.hound;
  configJson = with builtins; toFile "config.json" (toJSON {
    inherit (cfg) title repos;
    max-concurrent-indexers = cfg.maxConcurrentIndexers;
    dbpath = "/var/lib/hound";
    health-check-uri = "/healthz";
  });
in {
  options.services.depot.hound = with lib; {
    enable = mkOption {
      type        = types.bool;
      default     = false;
      description = ''
        Whether to enable the hound code search engine to forward
        journald logs to Stackdriver Logging.
      '';
    };

    repos = mkOption {
      type = lib.types.attrs;
      description = "Repository configuration for hound.";
    };

    port = mkOption {
      type = lib.types.int;
      default = 6080;
      description = "The port hound should listen on.";
    };

    title = mkOption {
      type = lib.types.str;
      description = "Page title for this hound instance";
    };

    maxConcurrentIndexers = mkOption {
      type = lib.types.int;
      default = 2;
    };
  };

  config = {
    systemd.services.hound = {
      description = "Code search engine";
      script = "${config.depot.third_party.hound}/bin/houndd -addr ':${toString cfg.port}' -conf '${configJson}'";
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.git ];

      serviceConfig = {
        Restart = "always";
        DynamicUser = true;
        StateDirectory = "hound";
        SupplementaryGroups = "git";
      };
    };
  };
}
