{ pkgs, depot, config, ... }:

let
  carpet = pkgs.fetchurl {
    url = "https://github.com/gnembon/fabric-carpet/releases/download/1.4.57/fabric-carpet-1.17.1-1.4.57+v220119.jar";
    sha256 = "1q3mnhcrjcp0i573645lyb92ldb8n0jhpjl4ydi1fbk9wqckxzcs";
  };

  carpet-extra = pkgs.fetchurl {
    url = "https://github.com/gnembon/carpet-extra/releases/download/1.4.57/carpet-extra-1.17.1-1.4.57.jar";
    sha256 = "1bliqby1h5wm7rdfrx6z698qb9acfvm3d1sjcvlw2q55x982g7gb";
  };

  userGroup = "minecraft";

  makeJvmOpts = megs: [
    "-Xms${toString megs}M"
    "-Xmx${toString megs}M"
  ];

  whitelist = {
    spreadwasser = "242a66eb-2df2-4585-9a28-ac763ad0d0f9";
    sternenseemann = "d8e48069-1905-4886-a5da-a4ee917ee254";
  };

  rconPasswordFile = config.age.secretsDir + "/minecraft-rcon";

  baseProperties = {
    white-list = true;
    allow-flight = true;
    difficulty = "hard";
    function-permission-level = 4;
    snooper-enabled = false;
    view-distance = 12;
    sync-chunk-writes = "false"; # the single biggest performance fix
    max-tick-time = 6000000; # TODO(sterni): disable watchdog via carpet
  };
in

{
  imports = [
    ../../modules/minecraft-fabric.nix
  ];

  config = {
    environment.systemPackages = [
      pkgs.mcrcon
      pkgs.jre
    ];

    users = {
      users."${userGroup}" = {
        isNormalUser = true;
        openssh.authorizedKeys.keys = depot.users.sterni.keys.all;
        shell = "${pkgs.fish}/bin/fish";
      };

      groups."${userGroup}" = { };
    };

    age.secrets = {
      minecraft-rcon.file = depot.users.sterni.secrets."minecraft-rcon.age";
    };

    services.minecraft-fabric-server = {
      creative = {
        enable = true;
        version = "1.17.1";
        mods = [
          carpet
          carpet-extra
        ];
        world = config.users.users.${userGroup}.home + "/worlds/creative";

        jvmOpts = makeJvmOpts 2048;
        user = userGroup;
        group = userGroup;

        inherit whitelist rconPasswordFile;
        ops = whitelist;

        serverProperties = baseProperties // {
          server-port = 25566;
          "rcon.port" = 25576;
          gamemode = "creative";
          enable-command-block = true;
          motd = "storage design server";
          spawn-protection = 2;
        };
      };

      carpet = {
        enable = true;
        version = "1.17.1";
        mods = [
          carpet
          carpet-extra
        ];
        world = config.users.users.${userGroup}.home + "/worlds/carpet";

        jvmOpts = makeJvmOpts 4096;
        user = userGroup;
        group = userGroup;

        inherit whitelist rconPasswordFile;
        ops = whitelist;

        serverProperties = baseProperties // {
          server-port = 25565;
          "rcon.port" = 25575;
          motd = "ich tu fleissig hustlen nenn mich bob der baumeister";

          level-seed = 7240251176989694927; # for posterity
        };
      };
    };
  };
}
