{ pkgs ? import <nixpkgs> {}, ... }:

{
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
    firewall.allowedTCPPorts = [ 9418 80 443 ];
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

    # Allow wpcarro to call nixos-rebuild
    trustedUsers = [ "root" "wpcarro" ];
  };

  ##############################################################################
  # Services
  ##############################################################################
  services.openssh.enable = true;

  services.lorri.enable = true;

  # TODO(wpcarro): Expose the Monzo credentials to this job. Currently they're
  # managed with direnv and pass, which presumably systemd isn't accessing.
  systemd.user.services.monzo-token-server = {
    enable = true;
    description = "Ensure my Monzo access token is valid";
    script = "/home/wpcarro/.nix-profile/bin/token-server";

    serviceConfig = {
      WorkingDirectory = "%h/briefcase/monzo_ynab";
      Type = "oneshot";
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
  security.acme.certs."wpcarro.dev".email = "wpcarro@gmail.com";

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

    virtualHosts.blog = {
      serverName = "blog.wpcarro.dev";
      useACMEHost = "wpcarro.dev";
      addSSL = true;
      extraConfig = ''
        location / {
          proxy_pass http://localhost:80
        }
      '';
    };
  };

  system.stateVersion = "20.09"; # Did you read the comment?
}
