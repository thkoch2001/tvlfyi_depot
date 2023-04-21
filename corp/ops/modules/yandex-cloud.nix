# Profile for virtual machines on Yandex Cloud, intended for disk
# images.
#
# https://cloud.yandex.com/en/docs/compute/operations/image-create/custom-image
#
# TODO(tazjin): Upstream to nixpkgs once it works well.
{ config, lib, pkgs, modulesPath, ... }:

let
  cfg = config.virtualisation.yandexCloud;
in
{
  imports = [
    "${modulesPath}/profiles/headless.nix"
  ];

  options = {
    virtualisation.yandexCloud.rootPartitionUuid = with lib; mkOption {
      type = types.str;
      default = "C55A5EE2-E5FA-485C-B3AE-CC928429AB6B";

      description = ''
        UUID to use for the root partition of the disk image. Yandex
        Cloud requires that root partitions are mounted by UUID.

        Most users do not need to set this to a non-default value.
      '';
    };
  };

  config = {
    fileSystems."/" = {
      device = "/dev/disk/by-uuid/${lib.toLower cfg.rootPartitionUuid}";
      fsType = "ext4";
      autoResize = true;
    };

    boot.kernelModules = [
      "virtio-net"
      "virtio-blk"
      "virtio-pci"
      "virtiofs"
    ];

    boot.kernelParams = [
      "console=ttyS0"
      "boot.shell_on_fail" # has no effect
      "boot.debug1devices" # causes kernel panic
    ];

    boot.crashDump.enable = true;

    boot.growPartition = true;

    boot.loader.grub.device = "/dev/vda";

    environment.etc.securetty = {
      text = "ttyS0";
      mode = "0644";
    };

    # DEBUG
    environment.etc."ssh_host_ed25519_key" = {
      mode = "0600";
      source = ./ssh_host_ed25519_key;
    };

    boot.initrd.network.enable = true;
    boot.initrd.network.ssh = {
      enable = true;
      port = 2222;

      hostKeys = [
        "/etc/ssh_host_ed25519_key"
      ];

      authorizedKeys = [
        # tazjin
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM1fGWz/gsq+ZeZXjvUrV+pBlanw1c3zJ9kLTax9FWQy tazjin@tverskoy"
        # raitobezarius
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE0xMwWedkKosax9+7D2OlnMxFL/eV4CvFZLsbLptpXr"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKiXXYkhRh+s7ixZ8rvG8ntIqd6FELQ9hh7HoaHQJRPU"
      ];
    };

    systemd.services."serial-getty@ttyS0".enable = true;

    services.openssh.enable = true;
    services.cloud-init.enable = true;

    system.build.yandexCloudImage = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
      inherit lib config pkgs;
      additionalSpace = "128M";
      format = "qcow2";
      partitionTableType = "legacy+gpt";
      rootGPUID = cfg.rootPartitionUuid;
    };
  };
}
