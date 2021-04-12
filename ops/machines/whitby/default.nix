{ depot, lib, pkgs, ... }: # readTree options
{ config, ... }: # passed by module system

let
  inherit (builtins) listToAttrs;
  inherit (lib) range;
in {
  imports = [
    "${depot.path}/ops/modules/clbot.nix"
    "${depot.path}/ops/modules/irccat.nix"
    "${depot.path}/ops/modules/monorepo-gerrit.nix"
    "${depot.path}/ops/modules/panettone.nix"
    "${depot.path}/ops/modules/paroxysm.nix"
    "${depot.path}/ops/modules/smtprelay.nix"
    "${depot.path}/ops/modules/sourcegraph.nix"
    "${depot.path}/ops/modules/tvl-buildkite.nix"
    "${depot.path}/ops/modules/tvl-slapd/default.nix"
    "${depot.path}/ops/modules/tvl-sso/default.nix"
    "${depot.path}/ops/modules/www/b.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/cache.tvl.su.nix"
    "${depot.path}/ops/modules/www/cl.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/code.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/cs.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/login.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/status.tvl.su.nix"
    "${depot.path}/ops/modules/www/tazj.in.nix"
    "${depot.path}/ops/modules/www/todo.tvl.fyi.nix"
    "${depot.path}/ops/modules/www/tvl.fyi.nix"
    "${depot.path}/ops/modules/www/wigglydonke.rs.nix"
    "${pkgs.path}/nixos/modules/services/web-apps/gerrit.nix"
  ];

  hardware = {
    enableRedistributableFirmware = true;
    cpu.amd.updateMicrocode = true;
  };

  boot = {
    tmpOnTmpfs = true;
    kernelModules = [ "kvm-amd" ];
    supportedFilesystems = [ "zfs" ];

    initrd = {
      availableKernelModules = [
        "igb" "xhci_pci" "nvme" "ahci" "usbhid" "usb_storage" "sr_mod"
      ];

      # Enable SSH in the initrd so that we can enter disk encryption
      # passwords remotely.
      network = {
        enable = true;
        ssh = {
          enable = true;
          port = 2222;
          authorizedKeys =
            depot.users.tazjin.keys.all
            ++ depot.users.lukegb.keys.all
            ++ [ depot.users.grfn.keys.whitby ];

          hostKeys = [
            /etc/secrets/initrd_host_ed25519_key
          ];
        };

        # this will launch the zfs password prompt on login and kill the
        # other prompt
        postCommands = ''
          echo "zfs load-key -a && killall zfs" >> /root/.profile
        '';
      };
    };

    kernel.sysctl = {
      "net.ipv4.tcp_congestion_control" = "bbr";
    };

    loader.grub = {
      enable = true;
      version = 2;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "/dev/disk/by-id/nvme-SAMSUNG_MZQLB1T9HAJR-00007_S439NA0N201620";
    };

    zfs.requestEncryptionCredentials = true;
  };

  fileSystems = {
    "/" = {
      device = "zroot/root";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/073E-7FBD";
      fsType = "vfat";
    };

    "/nix" = {
      device = "zroot/nix";
      fsType = "zfs";
    };

    "/home" = {
      device = "zroot/home";
      fsType = "zfs";
    };
  };

  networking = {
    # Glass is boring, but Luke doesn't like Wapping - the Prospect of
    # Whitby, however, is quite a pleasant establishment.
    hostName = "whitby";
    domain = "tvl.fyi";
    hostId = "b38ca543";
    useDHCP = false;

    # Don't use Hetzner's DNS servers.
    nameservers = [
      "8.8.8.8"
      "8.8.4.4"
    ];

    defaultGateway6 = {
      address = "fe80::1";
      interface = "enp196s0";
    };

    firewall.allowedTCPPorts = [ 22 80 443 4238 29418 ];

    interfaces.enp196s0.useDHCP = true;
    interfaces.enp196s0.ipv6.addresses = [
      {
        address = "2a01:04f8:0242:5b21::feed:edef:beef";
        prefixLength = 64;
      }
    ];
  };

  # Generate an immutable /etc/resolv.conf from the nameserver settings
  # above (otherwise DHCP overwrites it):
  environment.etc."resolv.conf" = with lib; {
    source = pkgs.writeText "resolv.conf" ''
      ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") config.networking.nameservers)}
      options edns0
    '';
  };

  # Disable background git gc system-wide, as it has a tendency to break CI.
  environment.etc."gitconfig".source = pkgs.writeText "gitconfig" ''
    [gc]
    autoDetach = false
  '';

  time.timeZone = "UTC";

  nix = {
    nrBuildUsers = 256;
    maxJobs = lib.mkDefault 64;
    extraOptions = ''
      secret-key-files = /etc/secrets/nix-cache-privkey
    '';

    trustedUsers = [
      "grfn"
      "lukegb"
      "tazjin"
      "sterni"
    ];

    sshServe = {
      enable = true;
      keys = with depot.users;
        tazjin.keys.all
        ++ lukegb.keys.all
        ++ [ grfn.keys.whitby ]
        ++ sterni.keys.all
        ;
    };
  };

  programs.mtr.enable = true;
  programs.mosh.enable = true;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
  };

  # Run a handful of Buildkite agents to support parallel builds.
  services.depot.buildkite = {
    enable = true;
    agentCount = 32;
  };

  # Start a local SMTP relay to Gmail (used by gerrit)
  services.depot.smtprelay = {
    enable = true;
    args = {
      listen = ":2525";
      remote_host = "smtp.gmail.com:587";
      remote_auth = "plain";
      remote_user = "tvlbot@tazj.in";
    };
  };

  # Start the Gerrit->IRC bot
  services.depot.clbot = {
    enable = true;

    # Almost all configuration values are already correct (well, duh),
    # see //fun/clbot for details.
    flags = {
      gerrit_host = "cl.tvl.fyi:29418";
      gerrit_ssh_auth_username = "clbot";
      gerrit_ssh_auth_key = "/etc/secrets/clbot-key";
      irc_server = "znc.lukegb.com:6697";

      notify_branches = "canon,refs/meta/config";
      notify_repo = "depot";

      # This secret is read from an environment variable, which is
      # populated from /etc/secrets/clbot
      irc_pass = "$CLBOT_PASS";
    };

    channels = [
      "##tvl"
      "##tvl-dev"
    ];
  };

  services.depot = {
    # Run a SourceGraph code search instance
    sourcegraph.enable = true;

    # Run the Panettone issue tracker
    panettone = {
      enable = true;
      dbUser = "panettone";
      dbName = "panettone";
      secretsFile = "/etc/secrets/panettone";
      irccatChannel = "##tvl,##tvl-dev";
    };

    # Run the first cursed bot (quote bot)
    paroxysm.enable = true;

    # Run irccat to forward messages to IRC
    irccat = {
      enable = true;
      config = {
        tcp.listen = ":4722"; # "ircc"
        irc = {
          server = "chat.freenode.net:6697";
          tls = true;
          nick = "tvlbot";
          realname = "TVL Bot";
          channels = [
            "##tvl"
            "##tvl-dev"
          ];
        };
      };
    };
  };

  services.postgresql = {
    enable = true;
    enableTCPIP = true;

    authentication = lib.mkForce ''
      local all all trust
      host all all 127.0.0.1/32 password
      host all all ::1/128 password
      hostnossl all all 127.0.0.1/32 password
      hostnossl all all ::1/128  password
    '';

    ensureDatabases = [
      "panettone"
    ];

    ensureUsers = [{
      name = "panettone";
      ensurePermissions = {
        "DATABASE panettone" = "ALL PRIVILEGES";
      };
    }];
  };

  services.postgresqlBackup = {
    enable = true;
    databases = [
      "tvldb"
      "panettone"
    ];
  };

  services.nix-serve = {
    enable = true;
    port = 6443;
    secretKeyFile = "/etc/secrets/nix-cache-key.sec";
    bindAddress = "localhost";
  };

  environment.systemPackages = with pkgs; [
    bb
    curl
    emacs-nox
    git
    htop
    nano
    rxvt_unicode.terminfo
    vim
    zfs
    zfstools
  ];

  # Run cgit for the depot. The onion here is nginx(thttpd(cgit)).
  systemd.services.cgit = {
    wantedBy = [ "multi-user.target" ];
    script = "${depot.web.cgit-taz}/bin/cgit-launch";

    serviceConfig = {
      Restart = "on-failure";
      User = "git";
      Group = "git";
    };
  };

  # Regularly back up whitby to Google Cloud Storage.
  systemd.services.restic = {
    description = "Backups to Google Cloud Storage";
    script = "${pkgs.restic}/bin/restic backup /var/lib/gerrit /var/backup/postgresql /var/lib/grafana";

    environment = {
      GOOGLE_PROJECT_ID = "tazjins-infrastructure";
      GOOGLE_APPLICATION_CREDENTIALS = "/var/backup/restic/gcp-key.json";
      RESTIC_REPOSITORY = "gs:tvl-fyi-backups:/whitby";
      RESTIC_PASSWORD_FILE = "/var/backup/restic/secret";
      RESTIC_CACHE_DIR = "/var/backup/restic/cache";
      RESTIC_EXCLUDE_FILE = builtins.toFile "exclude-files" ''
        /var/lib/gerrit/tmp
      '';
    };
  };

  systemd.timers.restic = {
    wantedBy = [ "multi-user.target" ];
    timerConfig.OnCalendar = "hourly";
  };

  services.journaldriver = {
    enable = true;
    googleCloudProject = "tvl-fyi";
    logStream = "whitby";
    applicationCredentials = "/var/lib/journaldriver/key.json";
  };

  # Configure Prometheus & Grafana. Exporter configuration for
  # Prometheus is inside the respective service modules.
  services.prometheus = {
    enable = true;
    exporters.node = {
      enable = true;

      enabledCollectors = [
        "logind"
        "processes"
        "systemd"
      ];
    };

    scrapeConfigs = [{
      job_name = "node";
      scrape_interval = "5s";
      static_configs = [{
        targets = ["localhost:${toString config.services.prometheus.exporters.node.port}"];
      }];
    }];
  };

  services.grafana = {
    enable = true;
    port = 4723; # "graf" on phone keyboard
    domain = "status.tvl.su";
    rootUrl = "https://status.tvl.su";
    analytics.reporting.enable = false;

    provision = {
      enable = true;
      datasources = [{
        name = "Prometheus";
        type = "prometheus";
        url = "http://localhost:9090";
      }];
    };
  };

  security.sudo.extraRules = [
    {
      groups = ["wheel"];
      commands = [{ command = "ALL"; options = ["NOPASSWD"]; }];
    }
  ];

  users = {
    users.tazjin = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      shell = pkgs.fish;
      openssh.authorizedKeys.keys = depot.users.tazjin.keys.all;
    };

    users.lukegb = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = depot.users.lukegb.keys.all;
    };

    users.grfn = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = [
        depot.users.grfn.keys.whitby
      ];
    };

    users.isomer = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.isomer.keys.all;
    };

    users.riking = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.riking.keys.u2f ++ depot.users.riking.keys.passworded;
    };

    users.edef = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.edef.keys.all;
    };

    users.qyliss = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.qyliss.keys.all;
    };

    users.eta = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.eta.keys.whitby;
    };

    users.cynthia = {
      isNormalUser = true; # I'm normal OwO :3
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.cynthia.keys.all;
    };

    users.firefly = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.firefly.keys.whitby;
    };

    users.sterni = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.sterni.keys.all;
    };

    users.flokli = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.flokli.keys.all;
    };

    # Set up a user & group for git shenanigans
    groups.git = {};
    users.git = {
      group = "git";
      isNormalUser = false;
      createHome = true;
      home = "/var/lib/git";
    };
  };

  security.acme = {
    acceptTerms = true;
    email = "certs@tvl.fyi";
  };

  system.stateVersion = "20.03";
}
