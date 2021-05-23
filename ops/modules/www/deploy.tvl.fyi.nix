{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  config = {
    systemd.services.ensure-deploy-dir = {
      description = "Ensure the html directory for deploy.tvl.fyi exists";
      wantedBy = [ "multi-user.target" ];
      script = [ (pkgs.writeShellScript ''
        mkdir -p /srv/www/deploy.tvl.fyi
      '') ];

      serviceConfig = {
        User = "nginx";
        Group = "nginx";
        Type = "oneshot";
      };
    };

    services.nginx.virtualHosts."deploy.tvl.fyi" = {
      enableACME = true;
      forceSSL = true;
      root = "/srv/www/deploy.tvl.fyi";
    };
  };
}
