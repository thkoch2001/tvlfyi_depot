{ depot, pkgs, lib, ... }:

let
  aerc-patched = pkgs.aerc.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      ./aerc-no-config-perms.patch
    ];
  });

  bins = depot.nix.getBins aerc-patched [ "aerc" ];

  config =
    depot.users.Profpatsch.importDhall.importDhall
      {
        root = ./.;
        files = [
          "aerc.dhall"
        ];
        main = "aerc.dhall";
        deps = [ ];
      }
      {
        aercFilter = name: "${aerc-patched}/share/aerc/filters/${name}";
        toIni = depot.users.Profpatsch.toINI { };
      };

  aerc-config = pkgs.linkFarm "alacritty-config" [
    {
      name = "aerc/accounts.conf";
      path = pkgs.writeText "accounts.conf" config.accounts;
    }
    {
      name = "aerc/aerc.conf";
      path = pkgs.writeText "aerc.conf" config.aerc;
    }
    {
      name = "aerc/binds.conf";
      path = pkgs.writeText "binds.conf" config.binds;
    }
  ];

  aerc = depot.nix.writeExecline "aerc" { } [
    "export"
    "XDG_CONFIG_HOME"
    aerc-config
    bins.aerc
    "$@"
  ];

in
aerc
