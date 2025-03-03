{ config, lib, pkgs, modulesPath, depot, ... }:

with lib;

{
  imports = [
    ../modules/common.nix
    (modulesPath + "/installer/scan/not-detected.nix")
    (depot.path.origSrc + "/ops/modules/prometheus-fail2ban-exporter.nix")
    (depot.path.origSrc + "/users/aspen/xanthous/server/module.nix")
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

  users.users.aspen.openssh.authorizedKeys.keys = [
    depot.users.aspen.keys.whitby
  ];

  age.secrets =
    let
      secret = name: depot.users.aspen.secrets."${name}.age";
    in
    {
      cloudflare.file = secret "cloudflare";

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

      windtunnel-bot-github-token = {
        file = secret "windtunnel-bot-github-token";
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

  security.acme.defaults.email = "root@gws.fyi";
  security.acme.acceptTerms = true;

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
    extraGroups = [ "docker" "keys" ];
  };
}
