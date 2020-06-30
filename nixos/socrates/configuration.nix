{ ... }:

let
  # TODO(wpcarro): Instead of importing these dependencies as parameters that
  # readTree will expose I need to import these dependencies manually because
  # I'm building this using `nixos-rebuild`. When I better understand how to
  # build socrates using readTree, prefer defining this as an anonymous
  # function.
  pkgs = import <nixpkgs> {};
  briefcase = import <briefcase> {};

  trimNewline = x: pkgs.lib.removeSuffix "\n" x;
  readSecret = x: trimNewline (builtins.readFile ("/etc/secrets/" + x));
in {
  imports = [ ./hardware.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "socrates";
    # The global useDHCP flag is deprecated, therefore explicitly set to false
    # here.  Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    useDHCP = false;
    networkmanager.enable = true;
    interfaces.enp2s0f1.useDHCP = true;
    interfaces.wlp3s0.useDHCP = true;
    firewall.allowedTCPPorts = [ 9418 80 443 6697 ];
  };

  time.timeZone = "UTC";

  programs.fish.enable = true;
  programs.mosh.enable = true;

  environment.systemPackages = with pkgs; [
    curl
    direnv
    emacs26-nox
    gnupg
    htop
    pass
    vim
    certbot
    tree
    git
  ];

  users = {
    # I need a git group to run the git server.
    groups.git = {};

    users.wpcarro = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      shell = pkgs.fish;
    };

    users.git = {
      group = "git";
      isNormalUser = false;
    };
  };

  nix = {
    # Expose depot as <depot>, nixpkgs as <nixpkgs>
    nixPath = [
      "briefcase=/home/wpcarro/briefcase"
      "depot=/home/wpcarro/depot"
      "nixpkgs=/home/wpcarro/nixpkgs"
    ];

    trustedUsers = [ "root" "wpcarro" ];
  };

  ##############################################################################
  # Services
  ##############################################################################

  systemd.services.bitlbee-stunnel = {
    description = "Provides TLS termination for Bitlbee.";
    wantedBy = [ "multi-user.target" ];
    unitConfig = {
      Restart = "always";
      User = "nginx"; # This is a hack to easily get certificate access.
    };
    script = let configFile = builtins.toFile "stunnel.conf" ''
      foreground = yes
      debug = 7

      [ircs]
      accept = 0.0.0.0:6697
      connect = 6667
      cert = /var/lib/acme/wpcarro.dev/full.pem
    ''; in "${pkgs.stunnel}/bin/stunnel ${configFile}";
  };

  nixpkgs.config.bitlbee.enableLibPurple = true;
  services.bitlbee = {
    interface = "0.0.0.0";
    authBackend = "pam";
    authMode = "Registered";
    enable = true;
    libpurple_plugins = [
      pkgs.telegram-purple
    ];
  };

  services.openssh.enable = true;

  services.gitea = {
    enable = true;
    # Without this the links to clone a repository like briefcase will be
    # "http://localhost:3000/wpcarro/briefcase".
    rootUrl = "https://git.wpcarro.dev/";
  };

  systemd.services.monzo-token-server = {
    enable = true;
    description = "Ensure my Monzo access token is valid";
    script = "${briefcase.monzo_ynab.tokens}/bin/token-server";

    # TODO(wpcarro): I'm unsure of the size of this security risk, but if a
    # non-root user runs `systemctl cat monzo-token-server`, they could read the
    # following, sensitive environment variables.
    environment = {
      store_path = "/var/cache/monzo_ynab";
      monzo_client_id = readSecret "monzo-client-id";
      monzo_client_secret = readSecret "monzo-client-secret";
      ynab_personal_access_token = readSecret "ynab-personal-access-token";
      ynab_account_id = readSecret "ynab-account-id";
      ynab_budget_id = readSecret "ynab-budget-id";
    };

    serviceConfig = {
      Type = "simple";
    };
  };

  services.gitDaemon = {
    enable = true;
    basePath = "/srv/git";
    exportAll = true;
    repositories = [ "/srv/git/briefcase" ];
  };

  # Since I'm using this laptop as a server in my flat, I'd prefer to close its
  # lid.
  services.logind.lidSwitch = "ignore";

  # Provision SSL certificates to support HTTPS connections.
  security.acme.acceptTerms = true;
  security.acme.email = "wpcarro@gmail.com";

  services.nginx = {
    enable = true;
    enableReload = true;

    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;

    commonHttpConfig = ''
      log_format json_combined escape=json
      '{'
          '"time_local":"$time_local",'
          '"remote_addr":"$remote_addr",'
          '"remote_user":"$remote_user",'
          '"request":"$request",'
          '"status": "$status",'
          '"body_bytes_sent":"$body_bytes_sent",'
          '"request_time":"$request_time",'
          '"http_referrer":"$http_referer",'
          '"http_user_agent":"$http_user_agent"'
      '}';
      access_log syslog:server=unix:/dev/log json_combined;
    '';

    virtualHosts = {
      "wpcarro.dev" = {
        addSSL = true;
        enableACME = true;
        root = briefcase.website;
      };
      "learn.wpcarro.dev" = {
        addSSL = true;
        enableACME = true;
        root = briefcase.website.learn;
      };
      "git.wpcarro.dev" = {
        addSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:3000";
        };
      };
      "blog.wpcarro.dev" = {
        addSSL = true;
        enableACME = true;
        root = briefcase.website.blog;
      };
      "sandbox.wpcarro.dev" = {
        addSSL = true;
        enableACME = true;
        root = briefcase.website.sandbox;
      };
      "learnpianochords.app" = {
        addSSL = true;
        enableACME = true;
        root = briefcase.website.sandbox.learnpianochords;
      };
    };
  };

  system.stateVersion = "20.09";
}
