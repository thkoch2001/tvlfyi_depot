# NixOS module to run Nixery, currently with local-storage as the
# backend for storing/serving image layers.
{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.nixery;
  description = "Nixery - container images on-demand";
in {
  options.services.depot.nixery = {
    enable = lib.mkEnableOption description;

    port = lib.mkOption {
      type = lib.types.int;
      default = 45243; # "image"
      description = "Port on which Nixery should listen";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.nixery = {
      inherit description;
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        DynamicUser = true;
        StateDirectory = "nixery";
        Restart = "always";
        ExecStart = "${depot.third_party.nixery.nixery-server}/bin/nixery";
      };

      environment = {
        PORT = toString cfg.port;
        NIXERY_PKGS_PATH = depot.path;
        NIXERY_STORAGE_BACKEND = "filesystem";
        NIX_TIMEOUT = "60"; # seconds

        # TODO(tazjin): Consider prefixing with the commit ID, to be
        # able to garbage-collect things a little easier?
        STORAGE_PATH = "/var/lib/nixery";
      };
    };
  };
}
