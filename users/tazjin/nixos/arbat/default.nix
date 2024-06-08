# arbat is my Unchartevice 6640MA, with a Zhaoxin CPU.
{ depot, lib, pkgs, ... }:

config:
let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
  usermod = name: depot.path.origSrc + ("/users/tazjin/nixos/modules/" + name);

  zdevice = device: {
    inherit device;
    fsType = "zfs";
  };
in
{
  imports = [
    (usermod "chromium.nix")
    (usermod "desktop.nix")
    (usermod "fonts.nix")
    # (usermod "hidpi.nix") # TODO(tazjin): not sure yet
    (usermod "home-config.nix")
    (usermod "laptop.nix")
    (usermod "persistence.nix")
    (usermod "physical.nix")
    (pkgs.home-manager.src + "/nixos")
  ];

  tvl.cache.enable = true;

  # TODO(tazjin): hardware settings; boot settings

  boot = {
    loader.systemd-boot.enable = true;
    supportedFilesystems = [ "zfs" ];
    zfs.devNodes = "/dev/";
  };

  networking = {
    hostName = "arbat";
    hostId = "864f050b";
    networkmanager.enable = true;
  };

  fileSystems = {
    "/" = zdevice "zpool/ephemeral/root";
    "/home" = zdevice "zpool/ephemeral/home";
    "/persist" = zdevice "zpool/persistent/data" // { neededForBoot = true; };
    "/nix" = zdevice "zpool/persistent/nix";
    "/depot" = zdevice "zpool/persistent/depot";

    # "/boot" = {
    #   device = "/dev/disk/by-uuid/2487-3908";
    #   fsType = "vfat";
    # };
  };

  # TODO(tazjin): decide on this
  # services.xserver.libinput.touchpad.clickMethod = "clickfinger";
  # services.xserver.libinput.touchpad.tapping = false;


  system.stateVersion = "24.11";
}
