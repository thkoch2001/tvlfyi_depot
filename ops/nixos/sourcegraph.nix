# Run sourcegraph, including its entire machinery, in a container.
# Running it outside of a container is a futile endeavour for now.
{ config, pkgs, lib, ... }:

let
  inherit (builtins) toJSON toFile;
  cfg = config.services.depot.sourcegraph;
  depot = config.depot;
  siteConfigJson = toJSON (toFile "sourcegraph-site-config.json" cfg.siteConfig);
  codeHostConfigJson = toJSON (toFile "sourcegraph-code-host-config.json" cfg.codeHostConfig);
in {
  options.services.depot.sourcegraph = with lib; {
    enable = mkEnableOption "SourceGraph code search engine";

    port = mkOption {
      description = "Port on which SourceGraph should listen";
      type = types.int;
      default = 3463;
    };

    cheddarPort = mkOption {
      description = "Port on which cheddar should listen";
      type = types.int;
      default = 4238;
    };

    siteConfig = mkOption {
      description = ''
        Site configuration for the SourceGraph instance. This is
        serialised to JSON.

        https://docs.sourcegraph.com/admin/config/advanced_config_file#site-configuration
      '';
      type = types.attrs;
    };

    codeHostConfig = mkOption {
      description = ''
        Code host configuration for fetching repositories. This is
        serialised to JSON.

        https://docs.sourcegraph.com/admin/config/advanced_config_file#code-host-configuration
      '';
    };

    # Note: There is also a "global configuration", however we have
    # not yet set anything in it.
  };

  config = lib.mkIf cfg.enable {
    # Run a cheddar syntax highlighting server
    systemd.services.cheddar-server = {
      wantedBy = [ "multi-user.target" ];
      script = "${depot.tools.cheddar}/bin/cheddar --listen 0.0.0.0:${toString cfg.cheddarPort} --sourcegraph-server";

      serviceConfig = {
        DynamicUser = true;
        Restart = "always";
      };
    };

    virtualisation.oci-containers.containers.sourcegraph = {
      image = "sourcegraph/server:3.26.0";

      ports = [
        "127.0.0.1:${toString cfg.port}:7080"
      ];

      volumes = [
        "/var/lib/sourcegraph/etc:/etc/sourcegraph"
        "/var/lib/sourcegraph/data:/var/opt/sourcegraph"
        "${siteConfigJson}:/etc/sourcegraph-site-config.json"
        "${codeHostConfigJson}:/etc/sourcegraph-code-host-config.json"
      ];

      environment = {
        SRC_SYNTECT_SERVER = "http://172.17.0.1:${toString cfg.cheddarPort}";
        SITE_CONFIG_FILE = "/etc/sourcegraph-site-config.json";
        EXTSVC_CONFIG_FILE = "/etc/sourcegraph-code-host-config.json";

        # These settings allow editing of configuration values via the
        # admin UI, which is not persisted back to disk.
        #
        # This is useful for experimenting with new settings.
        SITE_CONFIG_ALLOW_EDITS = "true";
        EXTSVC_CONFIG_ALLOW_EDITS = "true";
      };
    };
  };
}
