{ depot, pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
    ../profiles/archeology.nix
  ];

  environment.systemPackages = [
    depot.users.flokli.archeology.parse-bucket-logs
  ];

  networking.hostName = "archeology-ec2";

  system.stateVersion = "23.05"; # Did you read the comment?
}

