{ depot, pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  # Use the TVL binary cache
  tvl.cache.enable = true;

  networking.hostName = "archeology-ec2";

  services.clickhouse.enable = true;

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "cert-authority ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCvb/7ojfcbKvHIyjnrNUOOgzy44tCkgXY9HLuyFta1jQOE9pFIK19B4dR9bOglPKf145CCL0mSFJNNqmNwwavU2uRn+TQrW+U1dQAk8Gt+gh3O49YE854hwwyMU+xD6bIuUdfxPr+r5al/Ov5Km28ZMlHOs3FoAP0hInK+eAibioxL5rVJOtgicrOVCkGoXEgnuG+LRbOYTwzdClhRUxiPjK8alCbcJQ53AeZHO4G6w9wTr+W5ILCfvW4OmUXCX01sKzaBiQuuFCF6M/H4LlnsPWLMra2twXxkOIhZblwC+lncps9lQaUgiD4koZeOCORvHW00G0L39ilFbbnVcL6Itp/m8RRWm/xRxS4RMnsdV/AhvpRLrhL3lfQ7E2oCeSM36v1S9rdg6a47zcnpL+ahG76Gz39Y7KmVRQciNx7ezbwxj3Q5lZtFykgdfGIAN+bT8ijXMO6m68g60i9Bz4IoMZGkiJGqMYLTxMQ+oRgR3Ro5lbj7E11YBHyeimoBYXYGHMkiuxopQZ7lIj3plxIzhmUlXJBA4jMw9KGHdYaLhaicIYhvQmCTAjrkt2HvxEe6lU8iws2Qv+pB6tAGundN36RVVWAckeQPZ4ZsgDP8V2FfibZ1nsrQ+zBKqaslYMAHs01Cf0Hm0PnCqagf230xaobu0iooNuXx44QKoDnB+w== edef"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPTVTXOutUZZjXLB0lUSgeKcSY/8mxKkC0ingGK1whD2 flokli"
  ];

  system.stateVersion = "23.05"; # Did you read the comment?

  environment.systemPackages = [ pkgs.helix pkgs.kakoune pkgs.tmux ];
}

