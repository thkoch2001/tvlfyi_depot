{ depot, pkgs, ... }:

let
  inherit (depot.users) wpcarro;
  name = "diogenes";
  domainName = "billandhiscomputer.com";

  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
  usermod = name: depot.path.origSrc + ("/users/wpcarro/nixos/modules/" + name);
in
wpcarro.terraform.googleCloudVM {
  project = "wpcarros-infrastructure";
  name = "diogenes";
  region = "us-central1";
  zone = "us-central1-a";

  # DNS configuration
  extraConfig = {
    # billandhiscomputer.com
    resource.google_dns_managed_zone."${name}" = {
      inherit name;
      dns_name = "${domainName}.";
    };

    resource.google_dns_record_set."${name}" = {
      name = "${domainName}.";
      type = "A";
      ttl = 300; # 5m
      managed_zone = "\${google_dns_managed_zone.${name}.name}";
      rrdatas = [ "\${google_compute_instance.${name}.network_interface[0].access_config[0].nat_ip}" ];
    };

    resource.google_compute_instance."${name}" = {
      network_interface.access_config = {
        public_ptr_domain_name = "${domainName}.";
      };
    };
  };

  configuration = {
    imports = [
      (mod "quassel.nix")
      (usermod "nginx.nix")
      (usermod "www/billandhiscomputer.com.nix")
      (usermod "www/wpcarro.dev.nix")
    ];

    networking = {
      firewall.allowedTCPPorts = [
        22 # ssh
        80 # http
        443 # https
        6698 # quassel
      ];
      firewall.allowedUDPPortRanges = [
        { from = 60000; to = 61000; } # mosh
      ];
    };

    # Use the TVL binary cache
    tvl.cache.enable = true;

    users = {
      mutableUsers = true;
      users = {
        root = {
          openssh.authorizedKeys.keys = wpcarro.keys.all;
        };
        wpcarro = {
          isNormalUser = true;
          extraGroups = [ "wheel" "quassel" ];
          openssh.authorizedKeys.keys = wpcarro.keys.all;
          shell = pkgs.fish;
        };
        # This is required so that quasselcore can read the ACME cert in
        # /var/lib/acme, which is only available to user=acme or group=nginx.
        quassel.extraGroups = [ "nginx" ];
      };
    };

    security = {
      acme = {
        acceptTerms = true;
        defaults.email = "wpcarro@gmail.com";
      };

      sudo.wheelNeedsPassword = false;
    };

    programs = wpcarro.common.programs // {
      mosh.enable = true;
    };

    # I won't have an Emacs server running on diogenes, and I'll likely be in an
    # SSH session from within vterm. As such, Vim is one of the few editors that
    # I tolerably navigate this way.
    environment.variables = {
      EDITOR = "vim";
    };

    environment.systemPackages = wpcarro.common.shell-utils;

    services = wpcarro.common.services // {
      # TODO(wpcarro): Re-enable this when rebuild-system better supports
      # terraform deployments.
      # depot.auto-deploy = {
      #   enable = true;
      #   interval = "1h";
      # };

      # TODO(wpcarro): Re-enable this after debugging ACME and NXDOMAIN.
      depot.quassel = {
        enable = true;
        acmeHost = domainName;
        bindAddresses = [
          "0.0.0.0"
        ];
      };

      journaldriver = {
        enable = true;
        logStream = "home";
        googleCloudProject = "wpcarros-infrastructure";
        applicationCredentials = "/etc/gcp/key.json";
      };
    };

    system.stateVersion = "21.11";
  };
}
