# Configures an Apereo CAS instance for TVL SSO
{ depot, ... }:

let
  inherit (depot.third_party) apereo-cas;
in {
  config = {
    environment.systemPackages = [ apereo-cas ];
    systemd.services.apereo-cas = {
      description = "Apereo CAS Single Sign On server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      environment.JDK_JAVA_OPTIONS = "-Xmx512M -Xms512M";
      serviceConfig = {
        User = "apereo-cas";
        Group = "apereo-cas";
        ExecStart = "${apereo-cas}/bin/cas";
        EnvironmentFile = "/etc/cas/secrets";
        Restart = "always";
      };
    };
    users.users.apereo-cas = {
      isSystemUser = true;
      group = "apereo-cas";
    };
    users.groups.apereo-cas = {};
  };
}
