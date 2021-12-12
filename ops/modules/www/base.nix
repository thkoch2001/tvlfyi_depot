{ config, pkgs, ... }:

{
  config = {
    services.nginx = {
      enable = true;
      enableReload = true;

      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;

      appendHttpConfig = ''
        add_header Permissions-Policy "interest-cohort=()";
      '';
    };

    # NixOS 20.03 broke nginx and I can't be bothered to debug it
    # anymore, all solution attempts have failed, so here's a
    # brute-force fix.
    #
    # TODO(tazjin): Find a link to the upstream issue and see if
    # they've sorted it after ~20.09
    systemd.services.fix-nginx = {
      script =
        "${pkgs.coreutils}/bin/chown -f -R nginx: /var/spool/nginx /var/cache/nginx";

      serviceConfig = {
        User = "root";
        Type = "oneshot";
      };
    };

    systemd.timers.fix-nginx = {
      wantedBy = [ "multi-user.target" ];
      timerConfig = { OnCalendar = "minutely"; };
    };
  };
}
