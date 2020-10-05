{ config, lib, pkgs, modulesPath, ... }:

with lib;

{
  imports = [
    ../modules/common.nix
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  networking.hostName = "mugwump";

  boot = {
    loader.systemd-boot.enable = true;

    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];

    initrd = {
      availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [
        "uas" "usbcore" "usb_storage" "vfat" "nls_cp437" "nls_iso8859_1"
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
    groups = ["wheel"];
    commands = [{ command = "ALL"; options = ["NOPASSWD"]; }];
  }];

  services.fail2ban = {
    enable = true;
    ignoreIP = [
      "172.16.0.0/16"
    ];
  };

  services.openssh = {
    allowSFTP = false;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };

  services.grafana = {
    enable = true;
    port = 3000;
    domain = "metrics.gws.fyi";
    rootUrl = "https://metrics.gws.fyi";
    dataDir = "/var/lib/grafana";
    analytics.reporting.enable = false;

    provision = {
      enable = true;
      datasources = [{
        name = "Prometheus";
        type = "prometheus";
        url = "localhost:9090";
      }];
    };
  };

  security.acme.email = "root@gws.fyi";
  security.acme.acceptTerms = true;

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "metrics.gws.fyi" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.grafana.port}";
        };
      };
    };
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
      };
    };

    scrapeConfigs = [{
      job_name = "node";
      scrape_interval = "5s";
      static_configs = [{
        targets = ["localhost:${toString config.services.prometheus.exporters.node.port}"];
      }];
    }];
  };

  security.acme.certs."metrics.gws.fyi" = {
    dnsProvider = "namecheap";
    credentialsFile = "/etc/secrets/namecheap.env";
    webroot = mkForce null;
  };
}
