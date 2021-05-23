{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    systemd.services.ensure-deploys-dir = {
      description = "Ensure the html directory for deploys.tvl.fyi exists";
      wantedBy = [ "multi-user.target" ];
      script = "mkdir -p /var/html/deploys.tvl.fyi";

      serviceConfig = {
        User = "nginx";
        Group = "nginx";
        Type = "oneshot";
      };
    };

    services.nginx.virtualHosts."deploys.tvl.fyi" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/html/deploys.tvl.fyi";
    };
  };
}
