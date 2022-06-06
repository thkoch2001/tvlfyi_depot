{ ... }:

{
  imports = [
    ./nginx.nix
  ];

  config.services.fcgiwrap = {
    enable = true;
    socketType = "unix";
    socketAddress = "/run/fcgiwrap.sock";
    user = "http";
    group = "http";
  };
}
