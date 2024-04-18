# nixery.dev backing host in ru-central1-b
{ depot, lib, pkgs, ... }: # readTree options
{ config, ... }: # passed by module system

let
  mod = name: depot.path.origSrc + ("/ops/modules/" + name);
in
{
  imports = [
    (mod "known-hosts.nix")
    (mod "nixery.nix")
    (mod "tvl-users.nix")
    (mod "www/nixery.dev.nix")
    (mod "yandex-cloud.nix")

    (depot.third_party.agenix.src + "/modules/age.nix")
  ];

  networking = {
    hostName = "nixery-01";
    domain = "tvl.fyi";
    firewall.allowedTCPPorts = [ 22 80 443 ];
  };

  security.sudo.extraRules = lib.singleton {
    groups = [ "wheel" ];
    commands = [{ command = "ALL"; options = [ "NOPASSWD" ]; }];
  };

  services.depot.nixery.enable = true;

  # Automatically collect garbage from the Nix store.
  services.depot.automatic-gc = {
    enable = true;
    interval = "1 hour";
    diskThreshold = 25; # GiB
    maxFreed = "150"; # GiB
    preserveGenerations = "30d";
  };
}
