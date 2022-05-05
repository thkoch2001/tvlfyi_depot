{ depot, pkgs, lib, ... }:

let
  aerc-patched = pkgs.aerc.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      ./aerc-no-config-perms.patch
    ];
  });

  bins = depot.nix.getBins aerc-patched [ "aerc" ];

  config =
    depot.users.Profpatsch.importDhall.importDhall {
      root = ./.;
      files = [
        "aerc.dhall"
      ];
      main = "aerc.dhall";
      deps = [ ];
    }
    {
        concatNewline = lib.concatStringsSep "\n";
        aercFilter = name: "${aerc-patched}/share/aerc/filters/${name}";
        toIni = getSections:
          lib.generators.toINIWithGlobalSection {}
            (getSections {} toIniDhall);
    };

  toIniDhall = {
    newSection = {};
    add = key: val: sect: sect // { ${key} = val; };
    addAll = keyVals: sect: sect // builtins.listToAttrs keyVals;
    newSectionList = {};
    addSection = key: val: sect: sect // { ${key} = val; };
  };


  ini-file = name: ini: lib.pipe ini [
    (lib.generators.toINI { })
    (pkgs.writeText name)
  ];

  binds-file = name: binds: pkgs.writeText name binds;

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
      path = binds-file "binds.conf" config.binds;
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
