{ ... }:

{
  config = {
    users = {
      users.http = {
        isSystemUser = true;
        group = "http";
      };

      groups.http = { };
    };

    services.nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;

      user = "http";
      group = "http";

      appendHttpConfig = ''
        charset utf-8;
      '';
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];
  };
}
