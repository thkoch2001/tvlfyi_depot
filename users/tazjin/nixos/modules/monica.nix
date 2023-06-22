# Host the Monica personal CRM software.
{ depot, config, ... }:

{
  imports = [
    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  age.secrets.monica-appkey = {
    group = config.services.monica.group;
    file = depot.users.tazjin.secrets."monica-appkey.age";
    mode = "0440";
  };

  services.monica = {
    enable = true;
    hostname = "monica.tazj.in";
    appKeyFile = "/run/agenix/monica-appkey";
    database.createLocally = true;

    nginx = {
      enableACME = true;
      forceSSL = true;
    };
  };
}
