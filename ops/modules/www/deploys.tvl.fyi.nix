{ pkgs, ... }:

{
  imports = [ ./base.nix ];

  config = {
    # Ensure the directory for deployment diffs exists.
    systemd.tmpfiles.rules =
      [ "d /var/html/deploys.tvl.fyi/diff 0755 nginx nginx -" ];

    services.nginx.virtualHosts."deploys.tvl.fyi" = {
      enableACME = true;
      forceSSL = true;
      root = "/var/html/deploys.tvl.fyi";
    };

    services.depot.restic.paths = [ "/var/html/deploys.tvl.fyi" ];
  };
}
