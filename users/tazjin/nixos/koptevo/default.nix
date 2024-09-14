# NUC in my closet.
_: # ignore readTree options

{ config, depot, lib, pkgs, ... }:

let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
  usermod = name: depot.path.origSrc + ("/users/tazjin/nixos/modules/" + name);
in
{
  imports = [
    (mod "quassel.nix")
    (mod "www/base.nix")
    (usermod "airsonic.nix")
    (usermod "geesefs.nix")
    (usermod "homepage.nix")
    (usermod "miniflux.nix")
    (usermod "predlozhnik.nix")
    (usermod "tgsa.nix")
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.availableKernelModules = [ "ahci" "xhci_pci" "usb_storage" "sd_mod" "sdhci_pci" ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "nomodeset" ];
  };

  nix.settings.trusted-users = [ "tazjin" ];

  fileSystems = {
    "/" = {
      device = "rpool/root";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/E214-E6B3";
      fsType = "vfat";
    };

    "/var" = {
      device = "rpool/var";
      fsType = "zfs";
    };

    "/home" = {
      device = "rpool/home";
      fsType = "zfs";
    };
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;
  services.fwupd.enable = true;

  networking = {
    hostName = "koptevo";
    hostId = "07bbbf4f";
    domain = "tazj.in";
    useDHCP = true;
    firewall.enable = true;
    firewall.allowedTCPPorts = [ 22 80 443 8776 9443 ];

    wireless.enable = true;
    wireless.networks."How do I computer fast?" = {
      psk = "washyourface";
    };
  };

  time.timeZone = "UTC";

  security.acme = {
    acceptTerms = true;
    defaults.email = lib.mkForce "acme@tazj.in";

    # wildcard cert for usage with Yggdrasil services
    certs."y.tazj.in" = {
      dnsProvider = "yandexcloud";
      credentialFiles.YANDEX_CLOUD_IAM_TOKEN_FILE = "/run/agenix/lego-yandex";
      extraDomainNames = [ "*.y.tazj.in" ];

      # folder tvl/tazjin-private/default
      environmentFile = builtins.toFile "lego-yandex-env" ''
        YANDEX_CLOUD_FOLDER_ID=b1gq41rsbggeum4qafnh
      '';
    };
  };

  programs.fish.enable = true;

  users.users.tazjin = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "systemd-journal" ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
  };

  users.users.nginx.extraGroups = [ "acme" ];

  age.secrets =
    let
      secretFile = name: depot.users.tazjin.secrets."${name}.age";
    in
    {
      lego-yandex.file = secretFile "lego-yandex";
      tgsa-yandex.file = secretFile "tgsa-yandex";
    };

  security.sudo.wheelNeedsPassword = false;

  services.openssh.enable = true;

  services.depot.quassel = {
    enable = true;
    acmeHost = "koptevo.tazj.in";
    bindAddresses = [
      "0.0.0.0"
    ];
  };

  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server"; # for exit-node usage
  };

  # Automatically collect garbage from the Nix store.
  services.depot.automatic-gc = {
    enable = true;
    interval = "daily";
    diskThreshold = 15; # GiB
    maxFreed = 10; # GiB
    preserveGenerations = "14d";
  };

  services.nginx.virtualHosts."koptevo.tazj.in" = {
    addSSL = true;
    enableACME = true;

    extraConfig = ''
      location = / {
        return 302 https://at.tvl.fyi/?q=%2F%2Fusers%2Ftazjin%2Fnixos%2Fkoptevo%2Fdefault.nix;
      }
    '';
  };

  # I don't use the podcast nor playlist feature,
  # but I *have to* supply podcasts to gonic ...
  systemd.tmpfiles.rules = [
    "d /tmp/fake-podcasts 0555 nobody nobody -"
    "d /tmp/fake-playlists 0555 nobody nobody -"
  ];

  services.gonic = {
    enable = true;
    settings = {
      listen-addr = "0.0.0.0:4747";
      scan-interval = 5;
      scan-at-start-enabled = true;
      podcast-path = [ "/tmp/fake-podcasts" ];
      playlists-path = [ "/tmp/fake-playlists" ];
      music-path = [ "/var/lib/geesefs/tazjins-files/music" ];
    };
  };

  # hack to work around the strict sandboxing of the gonic module
  # breaking DNS resolution
  systemd.services.gonic.serviceConfig.BindReadOnlyPaths = [
    "-/etc/resolv.conf"
  ];

  # add a hard dependency on the FUSE mount
  systemd.services.gonic.requires = [ "geesefs.service" ];

  services.nginx.virtualHosts."music.tazj.in" = {
    addSSL = true;
    enableACME = true;

    locations."/" = {
      proxyPass = "http://127.0.0.1:4747";
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    bat
    curl
    emacs-nox
    htop
    jq
    nano
    nmap
    radicle-node
    wget
  ];

  # configure Yggdrasil network
  services.yggdrasil = {
    enable = true;
    persistentKeys = true;
    openMulticastPort = true;

    settings = {
      Listen = [ "tls://[::]:0" ];
      IfName = "ygg0";
      Peers = [
        "quic://ygg-msk-1.averyan.ru:8364"
        "tls://ekb.itrus.su:7992"
        "tls://s-mow-1.sergeysedoy97.ru:65534"
      ];

      MulticastInterfaces = [{
        Regex = "enp.*";
        Beacon = true;
        Listen = true;
        Port = 9443; # yggd
      }];

      AllowedPublicKeys = [
        "573fd89392e2741ead4edd85034c91c88f1e560d991bbdbf1fccb6233db4d325" # koptevo
      ];
    };
  };

  # TODO(tazjin): move this to a module for radicle stuff
  services.radicle = {
    enable = true;
    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILHs6jSvMdtu9oJCt48etEs8ExjfGY5PmWQsRzFleogS";
    privateKeyFile = "/etc/secrets/radicle"; # TODO: to manage, or not to manage ...

    settings = {
      web.pinned.repositories = [
        "rad:z2mdnBK1tX6pibdBfRct3ThCgheHu" # tvix-go
      ];

      node = {
        alias = "rad.tazj.in";
        seedingPolicy.default = "block";
      };
    };

    node = {
      openFirewall = true;
      listenAddress = "[::]:8776";
    };

    httpd = {
      enable = true;
      listenAddress = "127.0.0.1";
      listenPort = 7235; # radl
    };
  };

  services.nginx.virtualHosts."rad.tazj.in" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyPass = "http://127.0.0.1:7235";
  };

  services.nginx.virtualHosts."rad.y.tazj.in" = {
    enableSSL = true;
    useACMEHost = "y.tazj.in";
    locations."/".proxyPass = "http://127.0.0.1:7235";
  };

  programs.mtr.enable = true;
  programs.mosh.enable = true;
  zramSwap.enable = true;

  system.stateVersion = "23.05";
}
