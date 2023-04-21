# Profile for virtual machines on Yandex Cloud, intended for disk
#images.
#
# https://cloud.yandex.com/en/docs/compute/operations/image-create/custom-image
#
# TODO(tazjin): Upstream to nixpkgs once it works well.
{ pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/profiles/headless.nix"
  ];

  config = {
    boot.kernelModules = [
      "virtio-net"
      "virtio-blk"
      "virtio-pci"
      "virtiofs"
    ];

    boot.kernelParams = [
      "console=ttyS0"
    ];

    environment.etc.securetty = {
      text = "ttyS0";
      mode = "0644";
    };

    systemd.services."serial-getty@ttyS0".enable = true;

    services.openssh.enable = true;
    services.cloud-init.enable = true;

    system.build.yandexCloudImage = import (pkgs.path + "/nixos/lib/make-disk-image.nix") {
      inherit lib config pkgs;
      format = "qcow2";
    };
  };
}
