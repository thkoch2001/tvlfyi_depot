# This is a fork of the nixpkgs module for Harmonia, which adds compatibility
# with Nix 2.3.
#
# We will upstream this eventually.
{ config, pkgs, lib, ... }:
let
  cfg = config.services.depot.harmonia;
  format = pkgs.formats.toml { };

  credentials = lib.imap0
    (i: signKeyPath: {
      id = "sign-key-${builtins.toString i}";
      path = signKeyPath;
    })
    cfg.signKeyPaths;
in
{
  options = {
    services.depot.harmonia = {
      enable = lib.mkEnableOption "Harmonia: Nix binary cache written in Rust";

      signKeyPaths = lib.mkOption {
        type = lib.types.listOf lib.types.path;
        default = [ ];
        description = "Paths to the signing keys to use for signing the cache";
      };

      package = lib.mkPackageOption pkgs "harmonia" { };

      settings = lib.mkOption {
        inherit (format) type;
        default = { };
        description = ''
          Settings to merge with the default configuration.
          For the list of the default configuration, see <https://github.com/nix-community/harmonia/tree/master#configuration>.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.harmonia = {
      isSystemUser = true;
      group = "harmonia";
    };
    users.groups.harmonia = { };

    systemd.services.harmonia = {
      description = "harmonia binary cache service";

      requires = [ "nix-daemon.socket" ];
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        CONFIG_FILE = format.generate "harmonia.toml" cfg.settings;
        SIGN_KEY_PATHS = lib.strings.concatMapStringsSep " "
          (
            credential: "%d/${credential.id}"
          )
          credentials;
        # Note: it's important to set this for nix-store, because it wants to use
        # $HOME in order to use a temporary cache dir. bizarre failures will occur
        # otherwise
        HOME = "/run/harmonia";
      };

      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        User = "harmonia";
        Group = "harmonia";
        Restart = "on-failure";
        PrivateUsers = true;
        DeviceAllow = [ "" ];
        UMask = "0066";
        RuntimeDirectory = "harmonia";
        LoadCredential = builtins.map (credential: "${credential.id}:${credential.path}") credentials;
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
          "~@resources"
        ];
        CapabilityBoundingSet = "";
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ProtectKernelLogs = true;
        ProtectHostname = true;
        ProtectClock = true;
        RestrictRealtime = true;
        MemoryDenyWriteExecute = true;
        ProcSubset = "pid";
        ProtectProc = "invisible";
        RestrictNamespaces = true;
        SystemCallArchitectures = "native";
        PrivateNetwork = false;
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateMounts = true;
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        LockPersonality = true;
        RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6";
        LimitNOFILE = 65536;
      };
    };
  };
}

