# Run sourcegraph, including its entire machinery, in a container.
# Running it outside of a container is a futile endeavour for now.
{ config, pkgs, lib, ... }:

let
  cfg = config.services.depot.sourcegraph;
  depot = config.depot;
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
      image = "sourcegraph/server:3.18.0";

      ports = [
        "127.0.0.1:${toString cfg.port}:7080"
      ];

      volumes = [
        "/var/lib/sourcegraph/etc:/etc/sourcegraph"
        "/var/lib/sourcegraph/data:/var/opt/sourcegraph"
      ];

      environment.SRC_SYNTECT_SERVER = "http://172.17.0.1:${toString cfg.cheddarPort}";
    };
  };
}
