# Gerrit configuration for the TVL monorepo
{ pkgs, config, lib, ... }:

{
  services.gerrit = {
    enable = true;
    listenAddress = "[::]:4778"; # 4778 - grrt
    serverId = "4fdfa107-4df9-4596-8e0a-1d2bbdd96e36";
    settings = {
      core.packedGitLimit = "100m";
      log.jsonLogging = true;
      log.textLogging = false;
      # TODO: gitweb config
    };
  };
}
