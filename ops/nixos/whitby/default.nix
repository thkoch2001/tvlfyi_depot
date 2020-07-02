{ depot, lib, ... }:

let
  nixpkgs = import depot.third_party.nixpkgsSrc {};

  systemForConfig = configuration: (depot.third_party.nixos {
    inherit configuration;
  }).system;
in systemForConfig {
  inherit depot;
  imports = [
    "${depot.depotPath}/ops/nixos/depot.nix"
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
    defaultGateway6 = "fe80::1";

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
  };

  programs.mtr.enable = true;
  services.openssh.enable = true;

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

  users = {
    users.root.openssh.authorizedKeys.keys = [
      depot.users.tazjin.keys.frog
    ];

    users.tazjin = {
      isNormalUser = true;
      extraGroups = [ "git" "wheel" ];
      openssh.authorizedKeys.keys = [
        depot.users.tazjin.keys.frog
      ];
    };

    # Set up a user & group for git shenanigans
    groups.git = {};
    users.git = {
      group = "git";
      isNormalUser = false;
    };
  };

  system.stateVersion = "20.03";
}
