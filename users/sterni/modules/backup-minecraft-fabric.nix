# Companion module to minecraft-fabric.nix which automatically and regularly
# creates backups of all minecraft servers' worlds to a shared borg(1)
# repository.
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2023 sterni <sternenseemann@systemli.org>
{ pkgs, depot, config, lib, ... }:

let
  inherit (depot.nix) getBins;

  bins = getBins pkgs.borgbackup [ "borg" ]
    // getBins pkgs.mcrcon [ "mcrcon" ];

  unvaried = ls: builtins.all (l: l == builtins.head ls) ls;

  cfg = config.services.backup-minecraft-fabric-servers;

  instances = lib.filterAttrs (_: i: i.enable) config.services.minecraft-fabric-server;
  users = lib.mapAttrsToList (_: i: i.user) instances;
  groups = lib.mapAttrsToList (_: i: i.group) instances;

  mkBackupScript = instanceName: instanceCfg:
    let
      archivePrefix = "minecraft-fabric-${instanceName}-world-${builtins.baseNameOf instanceCfg.world}-";
    in

    pkgs.writeShellScript "backup-minecraft-fabric-${instanceName}" ''
      export MCRCON_HOST="localhost"
      export MCRCON_PORT="${toString instanceCfg.serverProperties."rcon.port"}"
      # Unfortunately, mcrcon can't read the password from a file
      export MCRCON_PASS="$(cat "''${CREDENTIALS_DIRECTORY}/${instanceName}-rcon-password")"

      ${bins.mcrcon} save-all
      unset MCRCON_PASS

      # Give the server plenty of time to save
      sleep 60

      ${bins.borg} ${lib.escapeShellArgs [
        "create"
        "--verbose" "--filter" "AMEU" "--list"
        "--stats" "--show-rc"
        "--compression" "zlib"
        "${cfg.repository}::${archivePrefix}{now}"
        instanceCfg.world
      ]}

      ${bins.borg} ${lib.escapeShellArgs [
        "prune"
        "--list"
        "--show-rc"
        "--glob-archives" "${archivePrefix}*"
        "--keep-hourly" "168"
        "--keep-daily" "31"
        "--keep-monthly" "6"
        "--keep-yearly" "2"
        cfg.repository
      ]}

      ${bins.borg} compact ${lib.escapeShellArg cfg.repository}
    '';
in

{
  imports = [
    ./minecraft-fabric.nix
  ];

  options = {
    services.backup-minecraft-fabric-servers = {
      enable = lib.mkEnableOption "backups of all Minecraft fabric servers";

      repository = lib.mkOption {
        type = lib.types.path;
        description = "Path to the borg(1) repository to use for all backups.";
        default = "/var/lib/backup/minecraft-fabric";
      };
    };
  };

  config = lib.mkIf (cfg.enable && builtins.length (builtins.attrNames instances) > 0) {
    assertions = [
      {
        assertion = unvaried users && unvaried groups;
        message = "all instances under services.minecraft-fabric-server must use the same user and group";
      }
    ];

    environment.systemPackages = [
      pkgs.borgbackup
    ];

    systemd = {
      services.backup-minecraft-fabric-servers = {
        description = "Backup world of all fabric based Minecraft servers";
        wantedBy = [ ];
        after = builtins.map
          (name: "minecraft-fabric-${name}.service")
          (builtins.attrNames instances);

        script = lib.concatStrings (lib.mapAttrsToList mkBackupScript instances);

        serviceConfig = {
          Type = "oneshot";
          User = builtins.head users;
          Group = builtins.head groups;
          LoadCredential = lib.mapAttrsToList
            (instanceName: instanceCfg: "${instanceName}-rcon-password:${instanceCfg.rconPasswordFile}")
            instances;
        };
      };

      timers.backup-minecraft-fabric-servers = {
        description = "Regularly backup Minecraft fabric servers";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-* 00/3:00:00";
          Persistent = true;
          RandomizedDelaySec = "1h";
        };
      };
    };
  };
}
