# Configures an Apereo CAS instance for TVL SSO
{ config, ... }:

let
  inherit (config.depot.third_party) apereo-cas;
in {
  config = {
    environment.systemPackages = [ apereo-cas ];
    systemd.services.apereo-cas = {
      description = "Apereo CAS Single Sign On server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        User = "apereo-cas";
        Group = "apereo-cas";
        ExecStart = "${apereo-cas}/bin/cas";
        Restart = "always";
      };
    };
    users.users.apereo-cas = {};
    users.groups.apereo-cas = {};
  };
}
