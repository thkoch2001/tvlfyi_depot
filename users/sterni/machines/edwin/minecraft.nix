{ pkgs, depot, config, ... }:

let
  carpet = pkgs.fetchurl {
    url = "https://github.com/gnembon/fabric-carpet/releases/download/1.4.56/fabric-carpet-1.18-1.4.56+v211130.jar";
    sha256 = "0rvl2yb8xymla8c052j07gqkqfkz4h5pxf6aip2v9v0h8r84p9hf";
  };

  carpet-extra = pkgs.fetchurl {
    url = "https://github.com/gnembon/carpet-extra/releases/download/1.4.56/carpet-extra-1.18-1.4.56.jar";
    sha256 = "16d8szz13wh5344d61sf0f3f7aj3qa35c6jrbd68krf3srpqfa4x";
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

# TODO(sterni): regular backups of carpet world

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
        version = "1.18";
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
        version = "1.18";
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
