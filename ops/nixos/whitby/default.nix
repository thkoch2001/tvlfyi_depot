{ depot, lib, ... }:

let
  inherit (builtins) listToAttrs;
  inherit (lib) range;

  nixpkgs = import depot.third_party.nixpkgsSrc {};

  # All Buildkite hooks are actually besadii, but it's being invoked
  # with different names.
  buildkiteHooks = depot.third_party.runCommandNoCC "buildkite-hooks" {} ''
    mkdir -p $out/bin
    ln -s ${depot.ops.besadii}/bin/besadii $out/bin/post-command
  '';
in lib.fix(self: {
  inherit depot;
  imports = [
    "${depot.depotPath}/ops/nixos/clbot.nix"
    "${depot.depotPath}/ops/nixos/depot.nix"
    "${depot.depotPath}/ops/nixos/monorepo-gerrit.nix"
    "${depot.depotPath}/ops/nixos/panettone.nix"
    "${depot.depotPath}/ops/nixos/paroxysm.nix"
    "${depot.depotPath}/ops/nixos/smtprelay.nix"
    "${depot.depotPath}/ops/nixos/sourcegraph.nix"
    "${depot.depotPath}/ops/nixos/tvl-slapd/default.nix"
    "${depot.depotPath}/ops/nixos/tvl-sso/default.nix"
    "${depot.depotPath}/ops/nixos/www/cl.tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/code.tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/cs.tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/login.tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/todo.tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/tvl.fyi.nix"
    "${depot.depotPath}/ops/nixos/www/b.tvl.fyi.nix"
    "${depot.third_party.nixpkgsSrc}/nixos/modules/services/web-apps/gerrit.nix"
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
          authorizedKeys = [
            depot.users.tazjin.keys.frog
          ];

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
    source = depot.third_party.writeText "resolv.conf" ''
      ${concatStringsSep "\n" (map (ns: "nameserver ${ns}") self.networking.nameservers)}
      options edns0
    '';
  };

  time.timeZone = "UTC";

  nix = {
    package = depot.third_party.nix;
    nrBuildUsers = 128;
    maxJobs = lib.mkDefault 64;
    extraOptions = ''
      secret-key-files = /etc/secrets/nix-cache-privkey
    '';

    trustedUsers = [
      "grfn"
      "lukegb"
      "tazjin"
    ];

    sshServe = {
      enable = true;
      keys = with depot.users;
        tazjin.keys.all
        ++ lukegb.keys.all
        ++ [ glittershark.keys.whitby ];
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
  services.buildkite-agents = listToAttrs (map (n: rec {
    name = "whitby-${toString n}";
    value = {
      inherit name;
      enable = true;
      tokenPath = "/etc/secrets/buildkite-agent-token";
      hooks.post-command = "${buildkiteHooks}/bin/post-command";
    };
  }) (range 1 8));

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
      irc_server = "qwerty.zxcvbnm.ninja:6697";

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
    };

    # Run the first cursed bot (quote bot)
    paroxysm.enable = true;
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

  environment.systemPackages = with nixpkgs; [
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
    script = "${nixpkgs.restic}/bin/restic backup /var/lib/gerrit /var/backup/postgresql";

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

  security.sudo.extraRules = [
    {
      groups = ["wheel"];
      commands = [{ command = "ALL"; options = ["NOPASSWD"]; }];
    }
  ];

  users = {
    users.root.openssh.authorizedKeys.keys = [
      depot.users.tazjin.keys.frog
    ];

    users.tazjin = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      shell = nixpkgs.fish;
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
        depot.users.glittershark.keys.whitby
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

    users.multi = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.multi.keys.whitby;
    };

    users.eta = {
      isNormalUser = true;
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.eta.keys.whitby;
    };

    users.v = {
      isNormalUser = true;  # Questionable...
      extraGroups = [ "git" ];
      openssh.authorizedKeys.keys = depot.users.v.keys.whitby;
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
    email = "mail@tazj.in";
  };

  system.stateVersion = "20.03";
})
