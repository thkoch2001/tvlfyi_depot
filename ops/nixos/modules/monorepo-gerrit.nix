# Gerrit configuration for the TVL monorepo
{ pkgs, config, lib, ... }:

let
  cfg = config.services.gerrit;
  gerritHooks = pkgs.runCommandNoCC "gerrit-hooks" {} ''
    mkdir -p $out/bin
    ln -s ${config.depot.ops.besadii}/bin/besadii $out/bin/ref-updated
  '';
in {
  services.gerrit = {
    enable = true;
    listenAddress = "[::]:4778"; # 4778 - grrt
    serverId = "4fdfa107-4df9-4596-8e0a-1d2bbdd96e36";
    builtinPlugins = [
      "download-commands"
      "hooks"
    ];

    settings = {
      core.packedGitLimit = "100m";
      log.jsonLogging = true;
      log.textLogging = false;
      sshd.advertisedAddress = "code.tvl.fyi:29418";
      hooks.path = "${gerritHooks}";

      # Configures gerrit for being reverse-proxied by nginx as per
      # https://gerrit-review.googlesource.com/Documentation/config-reverseproxy.html
      gerrit.canonicalWebUrl = "https://cl.tvl.fyi";
      httpd.listenUrl = "proxy-https://${cfg.listenAddress}";

      download.command = [
        "checkout"
        "cherry_pick"
        "format_patch"
        "pull"
      ];

      # Configure for cgit.
      gitweb = {
        type = "custom";
        url = "https://code.tvl.fyi";
        project = "/";
        revision = "/commit/?id=\${commit}";
        branch = "/log/?h=\${branch}";
        tag = "/tag/?h=\${tag}";
        roottree = "/tree/?h=\${commit}";
        file = "/tree/\${file}?h=\${commit}";
        filehistory = "/log/\${file}?h=\${branch}";
        linkname = "cgit";
      };

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

      # Email sending (emails are relayed via the tazj.in domain's
      # GSuite currently).
      #
      # Note that sendemail.smtpPass is stored in
      # $site_path/etc/secure.config and is *not* controlled by Nix.
      #
      # Receiving email is not currently supported.
      sendemail = {
        enable = true;
        html = false;
        connectTimeout = "10sec";
        from = "TVL Code Review <tvlbot@tazj.in>";
        includeDiff = true;
        smtpEncryption = "none";
        smtpServer = "localhost";
        smtpServerPort = 2525;
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
