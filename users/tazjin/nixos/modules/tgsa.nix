{ config, depot, lib, pkgs, ... }:

{
  systemd.services.tgsa = {
    description = "telegram -> SA bbcode thing";
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      DynamicUser = true;
      Restart = "always";
      LoadCredential = "tgsa-yandex.json:/run/agenix/tgsa-yandex";
    };

    script = ''
      export YANDEX_KEY_FILE="''${CREDENTIALS_DIRECTORY}/tgsa-yandex.json"
      ${depot.users.tazjin.tgsa}/bin/tgsa
    '';
  };

  services.nginx.virtualHosts."tgsa" = {
    serverName = "tgsa.tazj.in";
    enableACME = true;
    forceSSL = true;

    locations."/" = {
      proxyPass = "http://127.0.0.1:8472";
    };
  };
}
