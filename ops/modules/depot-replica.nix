# Configuration for receiving a depot replica from Gerrit's
# replication plugin.
#
# This only prepares the user and folder for receiving the replica,
# but Gerrit configuration still needs to be modified in addition.
{ config, depot, lib, pkgs, ... }:

let
  cfg = config.services.depot.replica;
in
{
  options.services.depot.replica = with lib; {
    enable = mkEnableOption "Receive depot git replica from Gerrit";

    key = mkOption {
      description = "Public key to use for replication";
      type = types.str;
      default = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFFab9O1xaQ1TCyn+CxmXHexdlLzURREG+UR3Qdi3BvH";
    };

    path = mkOption {
      description = "Replication destination path (will be created)";
      type = types.str;
      default = "/var/lib/depot";
    };
  };

  config = lib.mkIf cfg.enable {
    users.groups.depot = { };

    users.users.depot = {
      group = "depot";
      isSystemUser = true;
      createHome = true;
      home = cfg.path;
      homeMode = "750"; # group can read depot
      openssh.authorizedKeys.keys = lib.singleton cfg.key;
      shell = pkgs.bashInteractive; # gerrit needs to run shell commands
    };
  };
}
