{ depot, config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    ../modules/common.nix
    ../modules/development.nix
    "${modulesPath}/installer/scan/not-detected.nix"
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  system.stateVersion = "22.05";

  networking.hostName = "roswell";

  users.users.grfn.openssh.authorizedKeys.keys = [
    depot.users.grfn.keys.main
  ];

  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  services.openssh.passwordAuthentication = false;

  services.tailscale.enable = true;

  security.sudo.wheelNeedsPassword = false;
}
