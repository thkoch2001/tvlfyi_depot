{ config, lib, pkgs, modulesPath, depot, ... }:

with lib;

{
  imports = [
    ../modules/common.nix
    (modulesPath + "/installer/scan/not-detected.nix")
    (depot.path.origSrc + "/ops/modules/prometheus-fail2ban-exporter.nix")
    (depot.path.origSrc + "/users/grfn/xanthous/server/module.nix")
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  networking.hostName = "mugwump";

  system.stateVersion = "22.05";

  boot = {
    loader.systemd-boot.enable = true;

    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [
        "uas"
        "usbcore"
        "usb_storage"
        "vfat"
        "nls_cp437"
        "nls_iso8859_1"
      ];

      postDeviceCommands = pkgs.lib.mkBefore ''
        mkdir -m 0755 -p /key
        sleep 2
        mount -n -t vfat -o ro `findfs UUID=9048-A9D5` /key
      '';

      luks.devices."cryptroot" = {
        device = "/dev/disk/by-uuid/803a9028-339c-4617-a213-4fe138161f6d";
        keyFile = "/key/keyfile";
        preLVM = false;
      };
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/cryptroot";
      fsType = "btrfs";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/7D74-0E4B";
      fsType = "vfat";
    };
  };

  networking.interfaces = {
    enp0s25.useDHCP = false;
    wlp2s0.useDHCP = false;
  };

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 80 443 ];

  security.sudo.extraRules = [{
    groups = [ "wheel" ];
    commands = [{ command = "ALL"; options = [ "NOPASSWD" ]; }];
  }];

  nix.gc.dates = "monthly";

  age.secrets =
    let
      secret = name: depot.users.grfn.secrets."${name}.age";
    in
    {
      cloudflare.file = secret "cloudflare";
      ddclient-password.file = secret "ddclient-password";

      buildkite-ssh-key = {
        file = secret "buildkite-ssh-key";
        group = "keys";
        mode = "0440";
      };

      buildkite-token = {
        file = secret "buildkite-token";
        group = "keys";
        mode = "0440";
      };
    };

  services.fail2ban = {
    enable = true;
    ignoreIP = [
      "172.16.0.0/16"
    ];
  };

  services.openssh = {
    allowSFTP = false;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
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

  services.ddclient = {
    enable = true;
    domains = [ "home.gws.fyi" ];
    interval = "1d";
    zone = "gws.fyi";
    protocol = "cloudflare";
    username = "root@gws.fyi";
    passwordFile = config.age.secretsDir + "/ddclient-password";
    quiet = true;
  };

  systemd.services.ddclient.serviceConfig.DynamicUser = lib.mkForce false;

  security.acme.certs."metrics.gws.fyi" = {
    dnsProvider = "cloudflare";
    credentialsFile = config.age.secretsDir + "/cloudflare";
    webroot = mkForce null;
  };

  services.prometheus = {
    enable = true;
    exporters = {
      node = {
        enable = true;
        openFirewall = false;

        enabledCollectors = [
          "processes"
          "systemd"
          "tcpstat"
          "wifi"
        ];
      };

      nginx = {
        enable = true;
        openFirewall = true;
        sslVerify = false;
        constLabels = [ "host=mugwump" ];
      };

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
        static_configs = [{
          targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
      {
        job_name = "nginx";
        scrape_interval = "5s";
        static_configs = [{
          targets = [ "localhost:${toString config.services.prometheus.exporters.nginx.port}" ];
        }];
      }
      {
        job_name = "xanthous_server";
        scrape_interval = "1s";
        static_configs = [{
          targets = [ "localhost:${toString config.services.xanthous-server.metricsPort}" ];
        }];
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
        relabel_configs = [{
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
          }];
      }
    ];
  };

  services.xanthous-server.enable = true;

  virtualisation.docker = {
    enable = true;
    storageDriver = "btrfs";
  };

  services.buildkite-agents = listToAttrs (map
    (n: rec {
      name = "mugwump-${toString n}";
      value = {
        inherit name;
        enable = true;
        tokenPath = config.age.secretsDir + "/buildkite-token";
        privateSshKeyPath = config.age.secretsDir + "/buildkite-ssh-key";
        runtimePackages = with pkgs; [
          docker
          nix
          gnutar
          gzip
        ];
      };
    })
    (range 1 1));

  users.users."buildkite-agent-mugwump-1" = {
    isSystemUser = true;
    extraGroups = [ "docker" ];
  };
}
