{ depot, config, lib, pkgs, ... }:

with lib;

let
  nodesToScrape = [
    "ogopogo"
    # "dobharchu"
    "mugwump"
    # "yeren"
    "lusca"
  ];

  nodesRunningNginx = [
    "ogopogo"
    "mugwump"
  ];

  nodesRunningPostgres = [
    "ogopogo"
  ];

  blackboxTargets = [
    "https://gws.fyi"
    "https://windtunnel.ci"
    "https://app.windtunnel.ci"
    "https://metrics.gws.fyi"
  ];
in
{
  imports = [
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  config = {
    services.postgresql = {
      ensureUsers = [{
        name = config.services.grafana.settings.database.user;
        ensureDBOwnership = true;
      }];

      ensureDatabases = [
        config.services.grafana.settings.database.name
      ];
    };

    services.grafana = {
      enable = true;
      dataDir = "/var/lib/grafana";

      settings = {
        server = {
          http_port = 3000;
          root_url = "https://metrics.gws.fyi";
          domain = "metrics.gws.fyi";
        };
        analytics.reporting_enabled = false;

        database = {
          type = "postgres";
          user = "grafana";
          name = "grafana";
          host = "/run/postgresql";
        };
      };

      provision = {
        enable = true;
        datasources.settings.datasources = [{
          name = "Prometheus";
          type = "prometheus";
          url = "http://localhost:9090";
        }];
      };
    };

    security.acme.defaults.email = "root@gws.fyi";
    security.acme.acceptTerms = true;

    services.nginx = {
      enable = true;
      statusPage = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;

      virtualHosts = {
        "metrics.gws.fyi" = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://localhost:${toString config.services.grafana.settings.server.http_port}";
          };
        };
      };
    };

    age.secrets = {
      cloudflare.file = depot.users.aspen.secrets."cloudflare.age";
    };

    security.acme.certs."metrics.gws.fyi" = {
      dnsProvider = "cloudflare";
      credentialsFile = config.age.secretsDir + "/cloudflare";
      webroot = mkForce null;
    };

    services.prometheus = {
      enable = true;
      retentionTime = "30d";
      exporters = {
        blackbox = {
          enable = true;
          openFirewall = true;
          configFile = pkgs.writeText "blackbox-exporter.yaml" (builtins.toJSON {
            modules = {
              https_2xx = {
                prober = "http";
                http = {
                  method = "GET";
                  fail_if_ssl = false;
                  fail_if_not_ssl = true;
                  preferred_ip_protocol = "ip4";
                };
              };
            };
          });
        };
      };

      scrapeConfigs = [
        {
          job_name = "node";
          scrape_interval = "5s";
          static_configs =
            map
              (node: {
                targets = [ "${node}:${toString config.services.prometheus.exporters.node.port}" ];
                labels.node = node;
              })
              nodesToScrape;
        }
        {
          job_name = "nginx";
          scrape_interval = "5s";
          static_configs =
            map
              (node: {
                targets = [ "${node}:${toString config.services.prometheus.exporters.nginx.port}" ];
                labels.node = node;
              })
              nodesRunningNginx;
        }
        {
          job_name = "postgres";
          scrape_interval = "5s";
          static_configs =
            map
              (node: {
                targets = [ "${node}:${toString config.services.prometheus.exporters.postgres.port}" ];
                labels.node = node;
              })
              nodesRunningPostgres;
        }
        {
          job_name = "blackbox";
          metrics_path = "/probe";
          params.module = [ "https_2xx" ];
          scrape_interval = "5s";
          static_configs = [{
            targets = [
              "https://gws.fyi"
              "https://windtunnel.ci"
              "https://app.windtunnel.ci"
              "https://metrics.gws.fyi"
            ];
          }];
          relabel_configs = [
            {
              source_labels = [ "__address__" ];
              target_label = "__param_target";
            }
            {
              source_labels = [ "__param_target" ];
              target_label = "instance";
            }
            {
              target_label = "__address__";
              replacement = "localhost:${toString config.services.prometheus.exporters.blackbox.port}";
            }
          ];
        }
      ];
    };
  };
}
