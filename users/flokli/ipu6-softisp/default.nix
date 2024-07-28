# This file ensures the fixes from ./config.nix build with the version of
# nixpkgs from depot.
# If you're an outside user of this, import config.nix as a NixOS module (and
# check the README.md file).

{ depot
, pkgs
, ...
}:

let
  systemFor = sys: (depot.ops.nixos.nixosFor sys).system;
in
(depot.nix.readTree.drvTargets rec {
  testSystem = (systemFor ({ modulesPath, pkgs, ... }: {
    imports = [
      # Import the module, this is something a user would do in their config.
      ./config.nix
    ];

    # Make sure we use the linuxPackages_latest.
    boot.kernelPackages = pkgs.linuxPackages_latest;

    # Enable firmware.
    hardware.enableAllFirmware = true;

    # Set some options necessary to evaluate.
    boot.loader.systemd-boot.enable = true;
    fileSystems."/" = {
      device = "/dev/disk/by-partlabel/root";
      fsType = "xfs";
    };

    # Enable pipewire and wireplumber.
    services.pipewire = {
      enable = true;
      wireplumber.enable = true;
    };

    # Shut off the warning.
    system.stateVersion = "24.05";
  })) // {
    # 2024-07-28 aspen: The patches no longer cleanly apply on upstream
    meta.broken = true;
  };

  # Make sure the firmware requested by the driver is present in our firmware.
  # We do have a .zst suffix here, but that's fine, since request_firmware does
  # check ${name}.zst too in case CONFIG_FW_LOADER_COMPRESS is set.
  # The path needs to be kept in sync with the ones used in the kernel patch.
  checkFirmware = pkgs.runCommand "check-firmware" { } ''
    stat ${testSystem}/firmware/intel/ipu/ipu6se_fw.bin.zst
    stat ${testSystem}/firmware/intel/ipu/ipu6ep_fw.bin.zst
    stat ${testSystem}/firmware/intel/ipu/ipu6_fw.bin.zst
    stat ${testSystem}/firmware/intel/ipu/ipu6epmtl_fw.bin.zst

    # all good, succeed build
    touch $out
  '';
})
