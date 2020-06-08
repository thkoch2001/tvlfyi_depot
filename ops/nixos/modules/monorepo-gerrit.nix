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

      # Configures integration with the locally running OpenLDAP
      auth.type = "LDAP";
      ldap = {
        server = "ldap://localhost";
        accountBase = "ou=users,dc=tvl,dc=fyi";
        accountPattern = "(&(objectClass=organizationalPerson)(cn=\${username}))";
        accountFullName = "cn";
        accountEmailAddress = "mail";
        groupBase = "ou=groups,dc=tvl,dc=fyi";
        gerrit.canonicalWebUrl = "https://cl.tvl.fyi";

        # TODO(tazjin): Assuming this is what we'll be doing ...
        groupMemberPattern = "(&(objectClass=group)(member=\${dn}))";
      };
    };
  };
}
