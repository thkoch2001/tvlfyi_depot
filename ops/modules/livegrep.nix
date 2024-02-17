# Configures a code search instance using Livegrep.
#
# We do not currently build Livegrep in Nix, because it's a complex,
# multi-language Bazel build and doesn't play nicely with Nix.
{ config, lib, pkgs, ... }:

let
  cfg = config.services.depot.livegrep;

  livegrepConfig = {
    name = "livegrep";

    fs_paths = [{
      name = "depot";
      path = "/depot";
      metadata.url_pattern = "https://code.tvl.fyi/tree/{path}?id={version}#n{lno}";
    }];

    repositories = [{
      name = "depot";
      path = "/depot";
      revisions = [ "HEAD" ];

      metadata = {
        url_pattern = "https://code.tvl.fyi/tree/{path}?id={version}#n{lno}";
        remote = "https://cl.tvl.fyi/depot.git";
      };
    }];
  };

  configFile = pkgs.writeText "livegrep-config.json" (builtins.toJSON livegrepConfig);

  # latest as of 2024-02-17
  image = "ghcr.io/livegrep/livegrep/base:033fa0e93c";
in
{
  options.services.depot.livegrep = with lib; {
    enable = mkEnableOption "Run livegrep code search for depot";

    port = mkOption {
      description = "Port on which livegrep web UI should listen";
      type = types.int;
      default = 5477; # lgrp
    };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.oci-containers.containers.livegrep-codesearch = {
      inherit image;
      extraOptions = [ "--net=host" ];

      volumes = [
        "${configFile}:/etc/livegrep-config.json:ro"
        "/var/lib/gerrit/git/depot.git:/depot:ro"
      ];

      entrypoint = "/livegrep/bin/codesearch";
      cmd = [
        "-grpc"
        "0.0.0.0:5427" # lgcs
        "-reload_rpc"
        "-revparse"
        "/etc/livegrep-config.json"
      ];
    };

    virtualisation.oci-containers.containers.livegrep-frontend = {
      inherit image;
      dependsOn = [ "livegrep-codesearch" ];
      extraOptions = [ "--net=host" ];

      entrypoint = "/livegrep/bin/livegrep";
      cmd = [
        "-listen"
        "0.0.0.0:${toString cfg.port}"
        "-reload"
        "-connect"
        "localhost:5427"
        "-docroot"
        "/livegrep/web"
        # TODO(tazjin): docroot with styles etc.
      ];
    };

    systemd.services.livegrep-reindex = {
      script = "${pkgs.docker}/bin/docker exec -ti livegrep-codesearch bash";
      serviceConfig.Type = "oneshot";
    };

    systemd.paths.livegrep-reindex = {
      description = "Executes a livegrep reindex if depot refs change";
      wantedBy = [ "multi-user.target" ];

      pathConfig = {
        PathChanged = [
          "/var/lib/gerrit/git/depot.git/packed-refs"
          "/var/lib/gerrit/git/depot.git/refs"
        ];
      };
    };
  };
}


# sudo docker exec -ti livegrep /livegrep/bin/codesearch -reload_rpc -revparse /var/lib/livegrep/config.jsno
# sudo docker run -d --ip 172.17.0.3 --name livegrep -v /var/lib/livegrep:/varlib/livegrep -v /var/lib/gerrit/git/depot.git:/depot:ro -v /home/tazjin/livegrep-web:/livegrep/web:ro ghcr.io/livegrep/livegrep/base /livegrep/bin/livegrep -listen 0.0.0.0:8910 -reload -docroot /livegrep/webbsudo docker run -d --ip 172.17.0.3 --name livegrep -v /var/lib/livegrep:/varlib/livegrep -v /var/lib/gerrit/git/depot.git:/depot:ro -v /home/tazjin/livegrep-web:/livegrep/web:ro ghcr.io/livegrep/livegrep/base /livegrep/bin/livegrep -listen 0.0.0.0:8910 -reload -docroot /livegrep/webb
