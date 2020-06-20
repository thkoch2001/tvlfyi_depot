# Run sourcegraph, including its entire machinery, in a container.
# Running it outside of a container is a futile endeavour for now.
{ config, pkgs, lib, ... }:

let cfg = config.services.depot.sourcegraph;
in {
  options.services.depot.sourcegraph = {
    enable = lib.mkEnableOption "SourceGraph code search engine";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.oci-containers.containers.sourcegraph = {
      image = "sourcegraph/server:3.16.1";

      ports = [
        "127.0.0.1:3463:7080"
        "127.0.0.1:3370:3370"
      ];

      volumes = [
        "/var/lib/sourcegraph/etc:/etc/sourcegraph"
        "/var/lib/sourcegraph/data:/var/opt/sourcegraph"
      ];

      environment.SRC_SYNTECT_SERVER = "http://172.17.0.1:4238";
    };
  };
}
