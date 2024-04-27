{ depot, ... }:

{
  imports = [
    (depot.path.origSrc + "/ops/modules/quassel.nix")
  ];

  config = {
    services.depot.quassel = {
      enable = true;
      acmeHost = "sterni.lv";
      bindAddresses = [
        "0.0.0.0"
        "::"
      ];
    };
  };
}
