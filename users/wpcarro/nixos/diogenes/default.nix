{ depot, pkgs, ... }:
{ ... }:

let
  inherit (depot.users) wpcarro;
in {
  imports = [
    "${depot.path}/ops/modules/quassel.nix"
    (pkgs.path + "/nixos/modules/virtualisation/google-compute-image.nix")
  ];

  networking = {
    hostName = "diogenes";
    firewall.allowedTCPPorts = [
      80   # http
      443  # https
      6698 # quassel
    ];
  };

  # Use the TVL binary cache
  tvl.cache.enable = true;

  # Use 100G volume for /nix
  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/62396bde-9002-4025-83eb-2a6c731b7adc";
    fsType = "ext4";
  };

  users = {
    mutableUsers = true;
    users = {
      wpcarro = {
        isNormalUser = true;
        extraGroups = [ "wheel" "quassel" ];
        openssh.authorizedKeys.keys = wpcarro.keys.all;
        shell = pkgs.fish;
      };
    };
  };

  security = {
    acme = {
      acceptTerms = true;
      email = "wpcarro@gmail.com";
    };

    sudo.wheelNeedsPassword = false;
  };

  programs = wpcarro.common.programs // {
    mosh.enable = true;
  };

  # I won't have an Emacs server running on diogenes, and I'll likely be in an
  # SSH session from within vterm. As such, Vim is one of the few editors that I
  # tolerably navigate this way.
  environment.variables = {
    EDITOR = "vim";
  };

  environment.systemPackages = wpcarro.common.shell-utils;

  services = wpcarro.common.services // {
    depot.quassel = {
      enable = true;
      acmeHost = "wpcarro.dev";
      bindAddresses = [
        "0.0.0.0"
      ];
    };

    depot.auto-deploy = {
      enable = true;
      interval = "1h";
    };

    journaldriver = {
      enable = true;
      logStream = "home";
      googleCloudProject = "wpcarros-infrastructure";
      applicationCredentials = "/etc/gcp/key.json";
    };

    nginx = {
      enable = true;
      enableReload = true;

      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;

      # for journaldriver
      commonHttpConfig = ''
        log_format json_combined escape=json
        '{'
            '"remote_addr":"$remote_addr",'
            '"method":"$request_method",'
            '"host":"$host",'
            '"uri":"$request_uri",'
            '"status":$status,'
            '"request_size":$request_length,'
            '"response_size":$body_bytes_sent,'
            '"response_time":$request_time,'
            '"referrer":"$http_referer",'
            '"user_agent":"$http_user_agent"'
        '}';

        access_log syslog:server=unix:/dev/log,nohostname json_combined;
      '';

      virtualHosts = {
        "wpcarro.dev" = {
          addSSL = true;
          enableACME = true;
          root = wpcarro.website;
        };
        "blog.wpcarro.dev" = {
          addSSL = true;
          enableACME = true;
          root = wpcarro.website.blog;
        };
      };
    };
  };

  system.stateVersion = "21.11";
}
