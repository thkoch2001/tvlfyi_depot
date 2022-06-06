{ depot, lib, config, ... }:

let
  inherit (depot.users.sterni.external.flipdot-gschichtler)
    bahnhofshalle
    warteraum
    nixosModule
    ;
in

{
  imports = [
    nixosModule
    ./nginx.nix
  ];

  config = {
    age.secrets = lib.genAttrs [
      "warteraum-salt"
      "warteraum-tokens"
    ]
      (name: {
        file = depot.users.sterni.secrets."${name}.age";
      });

    services.flipdot-gschichtler = {
      enable = true;
      virtualHost = "flipdot.openlab-augsburg.de";
      packages = {
        inherit bahnhofshalle warteraum;
      };
      saltFile = config.age.secretsDir + "/warteraum-salt";
      tokensFile = config.age.secretsDir + "/warteraum-tokens";
    };
  };
}
