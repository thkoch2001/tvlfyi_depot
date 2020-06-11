# Gerrit configuration for the TVL monorepo
{ pkgs, config, lib, ... }:

let cfg = config.services.gerrit;
in {
  services.gerrit = {
    enable = true;
    listenAddress = "[::]:4778"; # 4778 - grrt
    serverId = "4fdfa107-4df9-4596-8e0a-1d2bbdd96e36";
    settings = {
      core.packedGitLimit = "100m";
      log.jsonLogging = true;
      log.textLogging = false;

      # Configures gerrit for being reverse-proxied by nginx as per
      # https://gerrit-review.googlesource.com/Documentation/config-reverseproxy.html
      gerrit.canonicalWebUrl = "https://cl.tvl.fyi";
      httpd.listenUrl = "proxy-https://${cfg.listenAddress}";
      sshd.listenAddress = [ ":29418" "[::]:29418" ];

      # Configures integration with the locally running OpenLDAP
      auth.type = "LDAP";
      ldap = {
        server = "ldap://localhost";
        accountBase = "ou=users,dc=tvl,dc=fyi";
        accountPattern = "(&(objectClass=organizationalPerson)(cn=\${username}))";
        accountFullName = "cn";
        accountEmailAddress = "mail";
        accountSshUserName = "cn";
        groupBase = "ou=groups,dc=tvl,dc=fyi";

        # TODO(tazjin): Assuming this is what we'll be doing ...
        groupMemberPattern = "(&(objectClass=group)(member=\${dn}))";
      };
    };
  };

  systemd.services.gerrit = {
    serviceConfig = {
      # There seems to be no easy way to get `DynamicUser` to play
      # well with other services (e.g. by using SupplementaryGroups,
      # which seem to have no effect) so we force the DynamicUser
      # setting for the Gerrit service to be disabled and reuse the
      # existing 'git' user.
      DynamicUser = lib.mkForce false;
      User = "git";
      Group = "git";
    };
  };
}
