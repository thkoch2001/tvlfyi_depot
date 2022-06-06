# Declarative, but low Nix module for a modded minecraft server using the
# fabric mod loader. That is to say, the build of the final server JAR
# is not encapsulated in a derivation.
#
# The module has the following interesting properties:
#
#   * The fabric installer is executed on each server startup to assemble the
#     patched server.jar. This is unfortunately necessary, as it seems to be
#     difficult to do so in a derivation (fabric-installer accesses the network,
#     the build doesn't seem to be reproducible). At least this avoids the
#     question of the patched jar's redistributability.
#   * RCON is used for starting and stopping which should prevent data loss,
#     since we can issue a manual save command.
#   * The entire runtime directory of the server is assembled from scratch on
#     each start, so only blessed state (like the world) and declarative
#     configuration (whitelist.json, server.properties, ...) survive.
#   * It supports more than one server running on the same machine.
#
# Missing features:
#
#   * Support for bans
#   * Support for mutable whitelist, ops, …
#   * Op levels
#
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2022 sterni <sternenseemann@systemli.org>

{ lib, pkgs, config, depot, ... }:

let
  #
  # Dependencies
  #
  inherit (depot.nix.utils) storePathName;
  inherit (depot.nix) getBins;

  bins = getBins pkgs.mcrcon [ "mcrcon" ]
    // getBins pkgs.jre [ "java" ]
    // getBins pkgs.diffutils [ "diff" ]
    // getBins pkgs.moreutils [ "sponge" ]
    // getBins pkgs.extrace [ "pwait" ]
    // getBins pkgs.util-linux [ "flock" ];

  #
  # Needed JARs
  #
  fetchJar = { pname, version, url, sha256, passthru ? { } }:
    pkgs.fetchurl {
      name = "${pname}-${version}.jar";
      inherit url sha256;
      passthru = passthru // { inherit version; };
    };

  fabricInstallerJar =
    fetchJar rec {
      pname = "fabric-installer";
      version = "0.11.0";
      url = "https://maven.fabricmc.net/net/fabricmc/fabric-installer/${version}/fabric-installer-${version}.jar";
      sha256 = "02ni9whjvd9lfadr2x7fahl4302b2z2xc6njgl86vfl29zm45fk8";
    };

  # log4j workaround for Minecraft Server >= 1.12 && < 1.17
  log4jFix_112_116 = pkgs.fetchurl {
    url = "https://launcher.mojang.com/v1/objects/02937d122c86ce73319ef9975b58896fc1b491d1/log4j2_112-116.xml";
    sha256 = "1paha357xbaffl38ckzgdh4l5iib2ydqbv7jsg67nj31nlalclr9";
  };

  serverJars = {
    # Manually updated list of known minecraft `server.jar`s for now.
    # Making this comprehensive isn't that interesting for now, since the module
    # is annoying to use outside of depot anyways as it uses //nix.
    "1.16.5" = fetchJar {
      pname = "server";
      version = "1.16.5";
      url = "https://launcher.mojang.com/v1/objects/1b557e7b033b583cd9f66746b7a9ab1ec1673ced/server.jar";
      sha256 = "19ix6x5ij4jcwqam1dscnqwm0m251gysc2j793wjcrb9sb3jkwsq";
      passthru = {
        baseJvmOpts = [
          "-Dlog4j.configurationFile=${log4jFix_112_116}"
        ];
      };
    };
  };

  #
  # mods directory for fabric
  #
  makeModFolder = name: mods:
    pkgs.runCommand "${name}-fabric-mod-folder" { } (
      ''
        mkdir -p "$out"
      '' + lib.concatMapStrings
        (mod: ''
          test -f "${mod}" || {
              printf 'Not a regular file: %s\n' "${mod}" >&2
              exit 1
          }
          ln -s "${mod}" "$out/${storePathName mod}"
        '')
        mods
    );

  #
  # Create a server.properties file
  #
  propertyValue = v:
    if builtins.isBool v
    then lib.boolToString v
    else toString v;

  serverPropertiesFile = name: instanceCfg:
    let
      serverProperties' =
        builtins.removeAttrs instanceCfg.serverProperties [
          "rcon.password"
        ] // {
          enable-rcon = true;
        };
    in
    pkgs.writeText "${name}-server.properties" (''
      # created by minecraft-fabric.nix
    '' + lib.concatStrings (lib.mapAttrsToList
      (key: value: ''
        ${key}=${propertyValue value}
      '')
      serverProperties'));

  #
  # Create JSON “state” files
  #
  writeJson = name: data: pkgs.writeText "${name}.json" (builtins.toJSON data);

  toWhitelist = name: uuid: { inherit name uuid; };

  whitelistFile = name: instanceCfg:
    writeJson "${name}-whitelist" (
      lib.mapAttrsToList toWhitelist instanceCfg.whitelist
    );

  opsFile = name: instanceCfg:
    writeJson "${name}-ops" (
      lib.mapAttrsToList
        (name: value:
          toWhitelist name value // {
            level = 4;
            bypassesPlayerLimit = true;
          }
        )
        instanceCfg.ops
    );

  #
  # Service start and stop scripts
  #
  stopScript = name: instanceCfg:
    pkgs.writeShellScript "minecraft-fabric-${name}-stop" ''
      set -eu

      # Before shutting down, display the diff between prescribed and used
      # server.properties file for debugging purposes; filter out credential
      actualProperties="''${RUNTIME_DIRECTORY}/server.properties"
      sort "$actualProperties" | ${bins.sponge} "$actualProperties"
      ( ${bins.diff} -u "${serverPropertiesFile name instanceCfg}" \
          "$actualProperties" \
          || true ) | grep -v rcon.password

      export MCRCON_HOST=localhost
      export MCRCON_PORT=${lib.escapeShellArg instanceCfg.serverProperties."rcon.port"}
      # Unfortunately, mcrcon can't read the password from a file
      export MCRCON_PASS="$(cat "''${CREDENTIALS_DIRECTORY}/rcon-password")"

      # Send stop request
      "${bins.mcrcon}" 'say Server is stopping' save-all stop

      # Wait for service to come down (systemd SIGTERMs right after ExecStop)
      "${bins.flock}" "''${RUNTIME_DIRECTORY}" true
    '';

  startScript = name: instanceCfg:
    let
      serverJar = serverJars.${instanceCfg.version} or
        (throw "Don't have server.jar for Minecraft Server ${instanceCfg.version}");

    in

    pkgs.writeShellScript "minecraft-fabric-${name}-start" ''
      set -eu

      cd "''${RUNTIME_DIRECTORY}"

      copyFromStore() {
          install -m600 "$1" "$2"
      }

      # Check if world is available
      if test ! -d "${instanceCfg.world}"; then
          echo "Could not find world, generating new one" >&2
          mkdir -p "${instanceCfg.world}"
      fi

      # Put required files into place
      echo eula=true > eula.txt
      ln -s "${instanceCfg.world}" "${instanceCfg.level-name or "world"}"
      copyFromStore "${serverJar}" server.jar
      copyFromStore "${whitelistFile name instanceCfg}" whitelist.json
      copyFromStore "${opsFile name instanceCfg}" ops.json
      ln -s "${makeModFolder name instanceCfg.mods}" mods

      # Create config and set password from credentials (echo hopefully doesn't leak)
      copyFromStore "${serverPropertiesFile name instanceCfg}" server.properties
      echo "rcon.password=$(cat "$CREDENTIALS_DIRECTORY/rcon-password")" >> server.properties

      # Build patched jar
      "${bins.java}" -jar "${fabricInstallerJar}" \
          server -mcversion "${instanceCfg.version}"

      # Lock is held as long as the server is running, so that we can wait for
      # the actual shutdown in the stop script without relying on $MAINPID.
      exec "${bins.flock}" "''${RUNTIME_DIRECTORY}" \
          "${bins.java}" \
          ${lib.escapeShellArgs (serverJar.baseJvmOpts ++ instanceCfg.jvmOpts)} \
          -jar fabric-server-launch.jar nogui
    '';

  #
  # Option types
  #
  impurePath = lib.types.path // {
    name = "impurePath";
    check = x:
      lib.types.path.check x
        && !(builtins.isPath x)
        && !(lib.hasPrefix builtins.storeDir (toString x));
  };


  instanceType = lib.types.submodule {
    options = {
      enable = lib.mkEnableOption "Minecraft server instance with the fabric mod loader";

      version = lib.mkOption {
        type = lib.types.str;
        description = "Minecraft Server version to use.";
        example = "1.16.5";
      };

      mods = lib.mkOption {
        type = with lib.types; listOf package;
        description = "List of fabric mod JARs to load.";
        default = [ ];
      };

      world = lib.mkOption {
        type = impurePath;
        description = "Path to the Minecraft world folder to use.";
        example = "/var/minecraft/world";
      };

      jvmOpts = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ ];
        example = [
          "-Xmx2048M"
          "-Xms2048M"
        ];
        description = ''
          Options to pass to
          <citerefentry>
            <refentrytitle>java</refentrytitle>
            <manvolnum>1</manvolnum>
          </citerefentry>
          in order to tweak the runtime of the JVM.
        '';
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "minecraft";
        description = ''
          Name of an existing user to run the server as. Needs to have write
          access to the specified world.
        '';
      };

      group = lib.mkOption {
        type = lib.types.str;
        default = "users";
        description = ''
          Name of an existing group to run the server under.
        '';
      };

      rconPasswordFile = lib.mkOption {
        type = impurePath;
        description = ''
          File (outised the store) that stores the password to use for Minecraft's
          RCON interface.
        '';
        example = "/var/secrets/minecraft-rcon";
      };

      whitelist = lib.mkOption {
        type = with lib.types; attrsOf str;
        description = ''
          Attribute set mapping whitelisted user names to their user ids.
        '';
        example = {
          sternenseemann = "d8e48069-1905-4886-a5da-a4ee917ee254";
        };
      };

      ops = lib.mkOption {
        type = with lib.types; attrsOf str;
        description = ''
          Attribute set mapping op-ed user names to their user ids.
          Setting permission levels is not possible at the moment,
          set to 4 by default.
        '';
        example = {
          sternenseemann = "d8e48069-1905-4886-a5da-a4ee917ee254";
        };
      };

      serverProperties = lib.mkOption {
        type = lib.types.submodule {
          freeformType = lib.types.attrs;

          # Only options the module needs to access are declared explicitly
          options = {
            server-port = lib.mkOption {
              type = lib.types.port;
              default = 25565;
              description = ''
                Port to listen on.
              '';
            };

            "rcon.port" = lib.mkOption {
              type = lib.types.port;
              default = 25575;
              description = ''
                Port to use for the RCON control mechanism.
              '';
            };
          };
        };
      };
    };
  };

  cfg = config.services.minecraft-fabric-server;

  serverPorts = lib.mapAttrsToList
    (_: instanceCfg:
      instanceCfg.serverProperties.server-port
    )
    cfg;

  rconPorts = lib.mapAttrsToList
    (_: instanceCfg:
      instanceCfg.serverProperties."rcon.port"
    )
    cfg;
in

{
  options = {
    services.minecraft-fabric-server = lib.mkOption {
      type = with lib.types; attrsOf instanceType;
      default = { };
      description = "Minecraft server instances with the fabric mod loader";
    };
  };

  config = {
    assertions = [
      {
        assertion = builtins.all (instance: !instance.enable) (builtins.attrValues cfg)
          || config.nixpkgs.config.allowUnfreeRedistributable or false
          || config.nixpkgs.config.allowUnfree or false;
        message = lib.concatStringsSep " " [
          "You need to allow unfree software for minecraft,"
          "as you'll implicitly agree to Mojang's EULA."
        ];
      }
      {
        assertion =
          let
            allPorts = serverPorts ++ rconPorts;
          in
          lib.unique allPorts == allPorts;
        message = "All assigned ports need to be unique.";
      }
    ];

    systemd.services = lib.mapAttrs'
      (name: instanceCfg:
        {
          name = "minecraft-fabric-${name}";
          inherit (instanceCfg) enable;
          value = {
            description = "Minecraft server ${name} with the fabric mod loader";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];

            serviceConfig = {
              Type = "simple";
              User = instanceCfg.user;
              Group = instanceCfg.group;
              ExecStart = startScript name instanceCfg;
              ExecStop = stopScript name instanceCfg;
              RuntimeDirectory = "minecraft-fabric-${name}";
              LoadCredential = "rcon-password:${instanceCfg.rconPasswordFile}";
              RestartSec = "40s";
            };
          };
        }
      )
      cfg;

    networking.firewall = {
      allowedTCPPorts = serverPorts;
      allowedUDPPorts = serverPorts;
    };
  };
}
