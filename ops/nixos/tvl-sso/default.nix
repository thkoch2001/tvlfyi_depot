# Configures an Apereo CAS instance for TVL SSO
{ config, lib, pkgs, ... }:

let
  inherit (pkgs) apereo-cas;
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
        ExecStart = "${apereo-cas}";
        Restart = "always";
      };
    };
    users.users.apereo-cas = {
      name = "apereo-cas";
      group = "apereo-cas";
    };
    users.groups.apereo-cas = {
      name = "apereo-cas";
    };
  };
}
