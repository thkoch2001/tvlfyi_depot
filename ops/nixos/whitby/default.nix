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
in {
  inherit depot;
  imports = [
    "${depot.depotPath}/ops/nixos/depot.nix"
    "${depot.depotPath}/ops/nixos/tvl-slapd/default.nix"
    "${depot.depotPath}/ops/nixos/tvl-sso/default.nix"
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
    hostId = "b38ca543";
    useDHCP = false;

    defaultGateway6 = {
      address = "fe80::1";
      interface = "enp196s0";
    };

    firewall.allowedTCPPorts = [ 22 80 443 ];

    interfaces.enp196s0.useDHCP = true;
    interfaces.enp196s0.ipv6.addresses = [
      {
        address = "2a01:04f8:0242:5b21::feed:edef:beef";
        prefixLength = 64;
      }
    ];
  };

  time.timeZone = "UTC";

  nix = {
    maxJobs = lib.mkDefault 64;
    extraOptions = ''
      secret-key-files = /etc/secrets/nix-cache-privkey
    '';

    trustedUsers = [
      "grfn"
      "lukegb"
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
  services.openssh.enable = true;

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

  environment.systemPackages = with nixpkgs; [
    bb
    curl
    emacs-nox
    git
    htop
    nano
    vim
    zfs
    zfstools
  ];

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

    # Set up a user & group for git shenanigans
    groups.git = {};
    users.git = {
      group = "git";
      isNormalUser = false;
    };
  };

  security.acme = {
    acceptTerms = true;
    email = "mail@tazj.in";

    certs."tvl.fyi" = {
      user = "nginx";
      group = "nginx";
      webroot = "/var/lib/acme/acme-challenge";
      postRun = "systemctl reload nginx";
      extraDomains = {
        "login.tvl.fyi" = null;
      };
    };
  };

  services.nginx = {
    enable = true;
    enableReload = true;

    recommendedTlsSettings = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;

    appendConfig = ''
      add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

      rewrite ^/builds/?$ https://buildkite.com/tvl/depot/ last;

      rewrite ^/monorepo-doc/?$ https://docs.google.com/document/d/1nnyByXcH0F6GOmEezNOUa2RFelpeRpDToBLYD_CtjWE/edit?usp=sharing last;

      rewrite ^/irc/?$ ircs://chat.freenode.net:6697/##tvl last;

      location ~* \.(webp|woff2)$ {
        add_header Cache-Control "public, max-age=31536000";
      }
    '';

    virtualHosts.tvl = {
      serverName = "tvl.fyi";
      useACMEHost = "tvl.fyi";
      root = depot.web.tvl;
      forceSSL = true;

      extraConfig = ''
        add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;

        rewrite ^/builds/?$ https://buildkite.com/tvl/depot/ last;

        rewrite ^/monorepo-doc/?$ https://docs.google.com/document/d/1nnyByXcH0F6GOmEezNOUa2RFelpeRpDToBLYD_CtjWE/edit?usp=sharing last;

        rewrite ^/irc/?$ ircs://chat.freenode.net:6697/##tvl last;

        location ~* \.(webp|woff2)$ {
          add_header Cache-Control "public, max-age=31536000";
        }
      '';
    };

    virtualHosts.login = {
      serverName = "login.tvl.fyi";
      useACMEHost = "tvl.fyi";
      forceSSL = true;

      extraConfig = ''
        location / {
          proxy_pass http://localhost:8443;
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header Host $host;
        }
      '';
    };
  };

  system.stateVersion = "20.03";
}
