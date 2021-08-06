# NixOS module to run Nixery, currently with local-storage as the
# backend for storing/serving image layers.
{ depot, config, lib, pkgs, ... }:

let
  cfg = config.services.depot.nixery;
  description = "Nixery - container images on-demand";
  storagePath = "/var/lib/nixery/${pkgs.nixpkgsCommits.unstable}";
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
        ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p ${storagePath}";
        ExecStart = "${depot.third_party.nixery.nixery-bin}/bin/nixery";
      };

      environment = {
        PORT = toString cfg.port;
        NIXERY_PKGS_PATH = "${depot.path}/.nixery";
        NIXERY_STORAGE_BACKEND = "filesystem";
        NIX_TIMEOUT = "60"; # seconds
        STORAGE_PATH = storagePath;
      };
    };
  };
}
